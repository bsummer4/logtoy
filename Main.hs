{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, LambdaCase #-}
{-# LANGUAGE OverloadedStrings, PolyKinds, ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies, TypeOperators, UnicodeSyntax              #-}

-- Handling of IOErrors probably deserves more attention, however warp
-- seems to do the right thing when it encounters unexpected IO errors. It
-- returns a 500 error and prints a warning to stderr.

-- `fsync()` might be worth consideration. Do we care about losing some
-- logs if the system crashes? I'm going to go with "no" for now.

-- `servent` ignores invalid query parameters.  For example, making
-- a request to '/read/test?level=warnn' returns all of the logs in
-- tests. This is pretty questionable! However, I'm going to keep this
-- behavior for now to simplify my code.

-- TODO Return the last n items, not the first n.
-- TODO For `?level=` return all levels at OR ABOVE this level.
-- TODO Stream log files with Conduit.
-- TODO Write more tests.
-- TODO queryLog is ugly.

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Control.Applicative
import Control.Monad

import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Char
import           Data.Maybe
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Format   as T
import qualified Data.Text.Lazy     as LT
import           Text.Printf

import Control.Concurrent.MVar

import qualified Data.List as L

import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J

import qualified Network.Wai              as W
import qualified Network.Wai.Handler.Warp as W

import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as P
import           System.Directory
import           System.Environment
import           System.IO
import           System.IO.Error

import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as A8

import Control.Monad.IO.Class
import Data.Proxy
import Servant


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

validLogNameChar ∷ Char → Bool
validLogNameChar c = (c == '-') || (isAlphaNum c && not(isUpper c))

-- To simplify pathname construction, we maintain the following invariants:
--   A `LogNames` must satisfy this regular expression: /^[a-z0-9-]+$/
--
-- Upper case characters are not allowed in order to avoid the edge-cases
-- that are introduced by case-sensitive file systems.
readLogName ∷ Text → Maybe LogName
readLogName "" = Nothing
readLogName t = if T.all validLogNameChar t
                then Just $ LogNameInternal t
                else Nothing

logNamePath ∷ LogName → P.FilePath
logNamePath (LogNameInternal i) = P.fromText i

instance FromText LogLevel where
  fromText s = case T.toLower s of
    "debug" → Just Debug
    "info"  → Just Info
    "warn"  → Just Warn
    "error" → Just Error
    _       → Nothing

instance FromText LogName where
  fromText = readLogName


-- Utilities -------------------------------------------------------------------

warn ∷ String → IO ()
warn = hPutStrLn stderr . ("Warning: " ++)

toMaybe ∷ Either a b → Maybe b
toMaybe (Left _)  = Nothing
toMaybe (Right r) = Just r

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
loadLogMsg bs = toMaybe $ flip A.parseOnly bs $ do
  let strToText = T.decodeUtf8 . BS.pack
  _ ← A8.char '['
  Just l ← fromText . T.decodeUtf8 <$> A8.takeWhile A8.isAlpha_ascii
  _ ← A8.char ']'
  m ← mkLogText . strToText <$> A.many' (A.notWord8 $ fromIntegral $ ord '\n')
  return $ LogMsg l m

dumpLogMsg ∷ LogMsg → ByteString
dumpLogMsg (LogMsg level (LogTextInternal msg)) =
  T.encodeUtf8 $ LT.toStrict $ case msg of
    "" → T.format "[{}]\n"    (T.Only $ show level)
    _  → T.format "[{}] {}\n" (show level, msg)


-- Locking Files ---------------------------------------------------------------

-- This is over-engineered. I was planning to have one lock per file,
-- but decided that it wasn't worth it since performance is not a goal.
withLogLock ∷ LogLock → LogName → IO a → IO a
withLogLock lock _ action =
  withMVar (unLogLock lock) (\() → action)


-- JSON Instances --------------------------------------------------------------

instance J.FromJSON LogText where
  parseJSON (J.String s) = return $ mkLogText s
  parseJSON _            = fail "Expected a string for the log text"

instance J.FromJSON LogLevel where
  parseJSON (J.String s) = fromMaybe (fail "Invalid log level") $
                             return <$> fromText s
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

appendLog ∷ (LogLock,P.FilePath) → Log → IO ()
appendLog (lk,logDir) (Log n m) =
  let fp = logDir </> logNamePath n
      msg = dumpLogMsg m
  in withLogLock lk n $ do
       createDirectoryIfMissing True $ P.encodeString logDir
       BS.appendFile (P.encodeString fp) msg

queryLog ∷ (LogLock,P.FilePath) → ReadQuery → IO [LogMsg]
queryLog (lk,logDir) (ReadQuery nm levelMay limitMay) = do
  let fp = P.encodeString $ logDir </> logNamePath nm

  allLogDataE ← tryIOError
              $ withLogLock lk nm
              $ BS.readFile fp

  case allLogDataE of
    Left e | isDoesNotExistError e → return []
           | otherwise             → ioError e
    Right allLogData → do
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

type Api = "read" :> Capture "logname" LogName
                  :> QueryParam "limit" Int
                  :> QueryParam "level" LogLevel
                  :> Get [Log]
      :<|> "log" :> ReqBody Log
                 :> Post Log

apiServer ∷ (LogLock,P.FilePath) → W.Application
apiServer config = serve (Proxy∷Proxy Api) $ readH :<|> logH
  where readH nm limit level = liftIO $
          map (Log nm) <$> queryLog config (ReadQuery nm level limit)

        logH entry = do
          _ ← liftIO $ appendLog config entry
          return entry


-- Testing ---------------------------------------------------------------------

instance Arbitrary LogMsg where
  arbitrary = LogMsg <$> arbitrary <*> arbitrary

instance Arbitrary LogText where
  arbitrary = mkLogText . T.pack <$> arbitrary

instance Arbitrary LogLevel where
  arbitrary = do
    -- XXX Icky. Can I use `toEnum`?
    let levels = enumerated
    i ← (`mod` length levels) <$> arbitrary
    return $ levels !! i

instance Arbitrary LogName where
  arbitrary = do
    r ← T.filter validLogNameChar . T.pack <$> arbitrary
    return $ fromMaybe (LogNameInternal "a") $ readLogName r

test ∷ IO ()
test = defaultMain $ testGroup "tests"
  [ testGroup "Log dumping/parsing"
      [ QC.testProperty "Just == loadLogMsg . dumpLogMsg" $
          \m → Just m == loadLogMsg (dumpLogMsg m)
      , testCase "dumpLogMsg" $
          dumpLogMsg (LogMsg Info (mkLogText "foo"))   @?= "[Info] foo\n"
      , testCase "dumpLogMsg on empty message" $
          dumpLogMsg (LogMsg Error (mkLogText ""))     @?= "[Error]\n"
      , testCase "Log messages are trimmed" $
          dumpLogMsg (LogMsg Warn (mkLogText " foo ")) @?= "[Warn] foo\n"
      ]
  ]


-- Entry Point -----------------------------------------------------------------

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
  W.run port $ apiServer (lk,logDir)
