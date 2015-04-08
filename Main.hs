{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax                                      #-}

-- # IO Edge Cases
-- TODO Catch exceptions caused by non-existent files.
-- TODO Where should log files be written?
-- TODO What if creating the log directory fails?

-- # Performance
-- TODO Stream log files with Conduit.
-- TODO Do I need to fsync() in order to not lose data on crash?

-- # Correctness
-- TODO Write more quickcheck tests.
-- TODO Enforce proper use of Content-Type.

-- # Code Quality
-- The request decoding code is super ugly.

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Monoid

import Test.QuickCheck

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.Format     as T
import qualified Data.Text.Lazy       as LT
import           Text.Printf

import Control.Concurrent.MVar

import qualified Data.List as L

import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J

import qualified Network.HTTP.Types       as W
import qualified Network.Wai              as W
import qualified Network.Wai.Handler.Warp as W

import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as P
import           System.Directory
import           System.Environment
import           System.IO


-- Types -----------------------------------------------------------------------

newtype LogText = LogTextInternal { unLogText ∷ Text }
  deriving (Eq,Show)

newtype LogName = LogNameInternal { unLogName ∷ Text }

data LogLevel = Debug | Info | Warn | Error
  deriving (Show,Eq,Enum,Bounded)

data LogMsg = LogMsg LogLevel LogText
  deriving (Eq,Show)

data ReadQuery = ReadQuery LogName (Maybe LogLevel) (Maybe Int)

data Log = Log LogName LogMsg

data Req = LogReq Log
         | ReadReq ReadQuery

data ReqError = InvalidEndpoint
              | InvalidQueryParam
              | InvalidBody Text

newtype LogLock = LogLockInternal { unLogLock ∷ MVar () }


-- Smart Constructors and Simple Instances -------------------------------------

-- To simplify parsing, we maintain the following invariants:
--   A `LogText` will never contain a newline.
--   A `LogText` will never have leading or trailing whitespace.
mkLogText ∷ Text → LogText
mkLogText = LogTextInternal . T.filter (/= '\n') . T.strip

-- To simplify pathname construction, we maintain the following invariants:
--   A `LogNames` must satisfy this regular expression: /^[a-z0-9-]+$/
--
-- Upper case characters are not allowed in order to avoid the edge-cases
-- that are introduced by case-sensitive file systems.
validLogNameChar c = (c == '-') || (isAlphaNum c && not(isUpper c))

readLogName ∷ Text → Maybe LogName
readLogName "" = Nothing
readLogName t = if T.all validLogNameChar t
                then Just $ LogNameInternal t
                else Nothing

logNamePath ∷ LogName → P.FilePath
logNamePath (LogNameInternal i) = P.fromText i

instance Read LogLevel where
  readsPrec _ s = case toLower <$> s of
    "debug" → [(Debug,"")]
    "info"  → [(Info,"")]
    "warn"  → [(Warn,"")]
    "error" → [(Error,"")]
    _       → []


-- Utilities -------------------------------------------------------------------

warn ∷ String → IO ()
warn = hPutStrLn stderr . ("Warning: " ++)

toEither ∷ a → Maybe b → Either a b
toEither l Nothing  = Left l
toEither _ (Just r) = Right r

safeRead ∷ Read a ⇒ String → Maybe a
safeRead t = case reads t of
               [(x,"")] → Just x
               _        → Nothing

mkAbsolute ∷ P.FilePath → P.FilePath → P.FilePath
mkAbsolute _  p | P.absolute p = p
mkAbsolute wd p                = P.collapse(wd </> p)

enumerated ∷ (Enum a, Bounded a) => [a]
enumerated = [minBound .. maxBound]


-- Lines in Log Files ----------------------------------------------------------

loadLogMsg ∷ ByteString → Maybe LogMsg
loadLogMsg logLineBS =
  let logLine = T.decodeUtf8 logLineBS
      safeIndex t i = if (i < T.length t) && (i >= 0)
                      then Just $ T.index t i
                      else Nothing

  in do

    '['      ← safeIndex logLine 0
    rBracket ← T.findIndex (== ']') logLine

    let logLevelWidth = rBracket - 1
        (bracketed,msgS) = T.splitAt (rBracket+1) logLine
        levelS = T.take logLevelWidth $ T.drop 1 bracketed

    level ← safeRead (T.unpack levelS)
    let msg = mkLogText msgS
    return $ LogMsg level msg

dumpLogMsg ∷ LogMsg → ByteString
dumpLogMsg (LogMsg level (LogTextInternal msg)) =
  T.encodeUtf8 $ LT.toStrict $ case msg of
    "" → T.format "[{}]\n"    (T.Only $ show level)
    _  → T.format "[{}] {}\n" (show level, msg)

prop_reversibleLogSerialization ∷ LogMsg → Bool
prop_reversibleLogSerialization m = Just m == loadLogMsg (dumpLogMsg m)


-- Locking Files ---------------------------------------------------------------

-- This is over-engineered, but I'm hoping it will help when I move away
-- from one-big-lock.
withLogLock ∷ LogLock → LogName → IO a → IO a
withLogLock lock _ action =
  withMVar (unLogLock lock) (\() → action)


-- Arbitrary Instances ---------------------------------------------------------

instance Arbitrary LogMsg where
  arbitrary = LogMsg <$> arbitrary <*> arbitrary

instance Arbitrary LogText where
  arbitrary = mkLogText <$> arbitrary

instance Arbitrary LogLevel where
  arbitrary = do
    -- XXX Icky. Can I use `toEnum`?
    let levels = enumerated
    i ← (`mod` length levels) <$> arbitrary
    return $ levels !! i

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary LogName where
  arbitrary = do
    r ← T.filter validLogNameChar <$> arbitrary
    return $ fromMaybe (LogNameInternal "a") $ readLogName r

-- JSON Instances --------------------------------------------------------------

instance J.FromJSON LogText where
  parseJSON (J.String s) = return $ mkLogText s
  parseJSON _            = fail "Expected a string for the log text"

instance J.FromJSON LogLevel where
  parseJSON (J.String s) = fromMaybe (fail "Invalid log level") $
                             return <$> safeRead (T.unpack s)
  parseJSON _            = fail "Expected a string for the log level"

instance J.FromJSON Log where
  parseJSON (J.Object v) = do logNameMay ← readLogName <$> v .: "name"
                              case logNameMay of
                                Nothing → fail "Invalid log name"
                                Just nm → Log nm <$>
                                            (LogMsg <$> v .: "level"
                                                    <*> v .: "msg")
  parseJSON _            = fail "Expected an object for the log"

instance J.ToJSON Log where
  toJSON (Log n (LogMsg level msg)) =
    J.object [ "name"  .= unLogName n
             , "level" .= show level
             , "msg"   .= unLogText msg
             ]


-- Log File IO -----------------------------------------------------------------

appendLog ∷ P.FilePath → LogLock → Log → IO ()
appendLog logDir lk (Log n m) =
  let fp = logDir </> logNamePath n
      msg = dumpLogMsg m
  in withLogLock lk n $ do
       createDirectoryIfMissing True $ P.encodeString logDir
       BS.appendFile (P.encodeString fp) msg

queryLog ∷ P.FilePath → LogLock → ReadQuery → IO [LogMsg]
queryLog logDir lk (ReadQuery nm levelMay limitMay) = do
  let fp = P.encodeString $ logDir </> logNamePath nm

  allLogData ← withLogLock lk nm $ BS.readFile fp

  let logLines = BS.split (fromIntegral $ ord '\n') allLogData
      logMsgs = loadLogMsg <$> logLines
      onlyMatching = (case limitMay of
                        Nothing → id
                        Just limit → take limit)
                   . (case levelMay of
                       Nothing → id
                       Just level → filter (\(LogMsg l _) → l == level))

  when (Nothing `L.elem` logMsgs) $
    warn $ printf "Log file contains an invalid line: %s" fp

  return $ onlyMatching $ catMaybes logMsgs


-- Server Stuff ----------------------------------------------------------------

getQueryParam ∷ Text → W.Query → Maybe Text
getQueryParam paramKey query = join $ cvt <$> L.find matches query

  where keyBS = T.encodeUtf8 paramKey

        matches ∷ W.QueryItem → Bool
        matches (bsK,Just _) = bsK == keyBS
        matches _            = False

        cvt ∷ W.QueryItem → Maybe Text
        cvt (_,valueBS) = T.decodeUtf8 <$> valueBS

decodeRequest ∷ W.Request → ByteString → Either ReqError Req
decodeRequest req body =
  let
    decodeLog ∷ Either ReqError Log
    decodeLog = left (InvalidBody . T.pack) $
                  J.eitherDecodeStrict' body

    pages ∷ Either ReqError (Maybe Int)
    pages = case getQueryParam "lines" (W.queryString req) of
      Just s  → case safeRead(T.unpack s) of
                  Nothing → Left InvalidQueryParam
                  Just x →  Right $ Just x
      Nothing → Right Nothing

    level ∷ Either ReqError (Maybe LogLevel)
    level = case getQueryParam "level" $ W.queryString req of
      Just s  → case safeRead (T.unpack s) of
                  Nothing → Left InvalidQueryParam
                  Just x  → Right $ Just x
      Nothing → Right Nothing

  in case W.pathInfo req of
    ["log"]          → LogReq <$> decodeLog
    ["read",logname] → case readLogName logname of
      Nothing → Left InvalidEndpoint
      Just nm → ReadReq <$> (ReadQuery nm <$> level <*> pages)
    _                → Left InvalidEndpoint

apiServer ∷ LogLock
          → P.FilePath
          → W.Request
          → (W.Response → IO W.ResponseReceived)
          → IO W.ResponseReceived
apiServer lk logDir req respond = do
  body ← LBS.toStrict <$> W.strictRequestBody req

  let okResp =  W.responseLBS W.status200 []
      failResp =  W.responseLBS W.status400 []

  case decodeRequest req body of
    Left InvalidEndpoint →
      respond $ W.responseLBS W.status400 [] "Invalid endpoint"

    Left InvalidQueryParam →
      respond $ W.responseLBS W.status400 [] "Invalid query parameter"

    Left (InvalidBody errMsg) →
      respond $ failResp msg
        where msg = "Couldn't parse request body: "
                 <> LBS.fromStrict(T.encodeUtf8 errMsg)

    Right (LogReq l) → do
      appendLog logDir lk l
      respond $ okResp $ LBS.fromStrict $ T.encodeUtf8 "{result: ok}"

    Right (ReadReq r@(ReadQuery nm _ _)) → do
      logs ← queryLog logDir lk r
      respond $ okResp $ J.encode $ Log nm <$> logs


-- Program Entry Points --------------------------------------------------------

test ∷ IO ()
test = quickCheck prop_reversibleLogSerialization

main ∷ IO ()
main = do
  progName ← getProgName
  args ← getArgs
  wd ← P.decodeString <$> getCurrentDirectory
  portS ← fromMaybe "8080" <$> lookupEnv "PORT"

  let logDir = mkAbsolute wd $ case args of
        [d] → P.decodeString d
        []  → "./logs"
        _   → error $ printf "usage: %s [logdir]" progName

  let port∷Int = fromMaybe badPort $ safeRead portS
        where badPort = error $ printf "Invalid port number: %s" portS

  lk ← LogLockInternal <$> newMVar ()
  W.run port $ apiServer lk logDir
