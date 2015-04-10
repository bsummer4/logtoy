{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, LambdaCase       #-}
{-# LANGUAGE OverloadedStrings, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, TypeOperators, UnicodeSyntax                    #-}

-- Handling of IOErrors probably deserves more attention, however warp
-- seems to do the right thing when it encounters unexpected IO errors. It
-- returns a 500 error and prints a warning to stderr.

-- `fsync()` might be worth consideration. Do we care about losing some
-- logs if the system crashes? I'm going to go with "no" for now.

-- `servant` ignores invalid query parameters.  For example, making
-- a request to '/read/test?level=warnn' returns all of the logs in
-- tests. This is pretty questionable! However, I'm going to keep this
-- behavior for now to simplify my code.

-- TODO `servant` sends shitty response texts when there's an exception.
--      Handle them manually.
-- TODO Stream JSON text directly to the WAI response using `responseStream`.
-- TODO Look into reading the log file back-to-front. The current approach
--      will not scale to larger log files.

module Main (main,test) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Proxy

import           Data.Aeson                       ((.:), (.=))
import qualified Data.Aeson                       as J
import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.Char
import           Data.Maybe
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Format                 as T
import qualified Data.Text.Lazy                   as LT
import           Text.Printf

import Control.Concurrent.MVar

import qualified Data.Vector                 as BV
import           Data.Vector.Generic         ((!))
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as VM

import           Conduit
import qualified Data.Conduit        as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as C

import qualified Network.Wai              as W
import qualified Network.Wai.Handler.Warp as W

import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as P
import           System.Directory
import           System.Environment
import           System.IO.Error
import           System.Posix.Files

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Servant


-- Types -----------------------------------------------------------------------

newtype LogText = LogTextInternal { unLogText ∷ Text }
  deriving (Eq,Show)

newtype LogName = LogNameInternal { unLogName ∷ Text }

data LogLevel = Debug | Info | Warn | Error
  deriving (Show,Eq,Ord,Enum,Bounded)

data LogMsg = LogMsg LogLevel LogText
  deriving (Eq,Show)

data ReadQuery = ReadQuery LogName (Maybe LogLevel) (Maybe Int)

data Log = Log LogName LogMsg

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

toMaybe ∷ Either a b → Maybe b
toMaybe (Left _)  = Nothing
toMaybe (Right r) = Just r

safeRead ∷ Read a => String → Maybe a
safeRead t = case reads t of
               [(x,"")] → Just x
               _        → Nothing

mkAbsolute ∷ P.FilePath → P.FilePath → P.FilePath
mkAbsolute _  p | P.absolute p = p
mkAbsolute wd p                = P.collapse(wd </> p)

enumerated ∷ (Enum a, Bounded a) => [a]
enumerated = [minBound .. maxBound]

rotateL ∷ (V.Vector v a) => Int → v a → v a
rotateL offset vec =
  V.generate sz $ \i → vec ! mod (i + offset) sz
    where sz = V.length vec

-- Using a ring buffer.
sinkVecLastN ∷ (MonadBase base m, V.Vector v a, PrimMonad base)
                => Int -- ^ maximum allowed size
                → Consumer a m (v a)
sinkVecLastN maxSize | maxSize <= 0 = return V.empty
sinkVecLastN maxSize                = do
    mv <- liftBase $ VM.new maxSize
    let go n = do
            let i = n `mod` maxSize
            mx <- await
            case mx of
                Nothing -> do
                    let frozen = liftBase (V.unsafeFreeze mv)
                    if n < maxSize then V.slice 0 i <$> frozen
                                   else rotateL i <$> frozen
                Just x -> do
                    liftBase $ VM.write mv i x
                    go (n + 1)
    go 0

lastNC ∷ (MonadBase base m, PrimMonad base) => Int → Conduit a m a
lastNC n = sinkVecLastN n >>= BV.mapM_ yield

lastN ∷ Int → [a] → [a]
lastN n xs = runST $ C.runConduit $ C.sourceList xs $= lastNC n $$ sinkList

fuseMaybe ∷ Monad m => Conduit a m b → Maybe (Conduit b m b) → Conduit a m b
fuseMaybe source Nothing  = source
fuseMaybe source (Just c) = source $= c


-- Lines in Log Files ----------------------------------------------------------

readLogMsg ∷ ByteString → Maybe LogMsg
readLogMsg bs = toMaybe $ flip A.parseOnly bs $ do
    let strToText = T.decodeUtf8 . BS.pack
    _ ← A8.char '['
    Just l ← fromText . T.decodeUtf8 <$> A8.takeWhile A8.isAlpha_ascii
    _ ← A8.char ']'
    m ← mkLogText . strToText <$> A.many' (A.notWord8 $ fromIntegral $ ord '\n')
    return $ LogMsg l m

showLogMsg ∷ LogMsg → ByteString
showLogMsg (LogMsg level (LogTextInternal msg)) =
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

-- If we don't have permissions to read a file, getFileStatus tells us
-- that the file size is zero. This is annoying, because it
-- `CB.sourceFile` wont open the file if we ask for zero bytes from
-- it. This causes permission errors to go unreported.
--
-- Since we want an exception to be thrown when we try to read from
-- files that we don't have permissions for, we return 1 instead. This is
-- NOT a good solution, and needs to be cleaned up.
getLogFileSize ∷ (LogLock,P.FilePath) → LogName → IO (Maybe Int)
getLogFileSize (lk,logDir) nm = do
    let fp = P.encodeString $ logDir </> logNamePath nm
    filesizeE ← withLogLock lk nm
              $ tryIOError
              $ fromIntegral . fileSize <$> getFileStatus fp
    case filesizeE of
      Left e | isDoesNotExistError e → return Nothing
      Left e | otherwise             → ioError e
      Right 0                        → return $ Just 1 -- TODO HACK
      Right sz                       → return $ Just sz

logLevelGE ∷ Monad m => LogLevel → Conduit Log m Log
logLevelGE minLevel = filterC $ \l → logLevel l >= minLevel
  where logLevel (Log _ (LogMsg l _)) = l

queryLogs ∷ (MonadBase base m,PrimMonad base)
          ⇒ ReadQuery
          → Conduit ByteString m Log
queryLogs (ReadQuery nm levelMay limitMay) =
  CB.lines $= C.mapMaybe (fmap (Log nm) . readLogMsg)
    `fuseMaybe` (logLevelGE <$> levelMay)
    `fuseMaybe` (lastNC     <$> limitMay)


-- Server Stuff ----------------------------------------------------------------

type Api = "read" :> Capture "logname" LogName
                  :> QueryParam "limit" Int
                  :> QueryParam "level" LogLevel
                  :> Get [Log]
      :<|> "log" :> ReqBody Log
                 :> Post Log

apiServer ∷ (LogLock,P.FilePath) → W.Application
apiServer config@(lk,logDir) = serve (Proxy∷Proxy Api) $ readH :<|> logH

  where readH nm limit level = liftIO $ do
            -- Here, we rely on the fact that our IO files are append-only
            -- to keep the lock duraction very short. We use the lock only to
            -- grab the size of the file, and then we stream the first n
            -- bytes of the file. This way, even if the end of the file goes
            -- into an inconsistent state while we are reading, this doesn't
            -- affect us.
            szM ← getLogFileSize config nm
            let fp = P.encodeString (logDir </> logNamePath nm)
            case szM of
              Nothing → return []
              Just sz → do
                let q = ReadQuery nm level limit
                runResourceT $ C.runConduit $
                  CB.sourceFile fp $= takeCE sz $= queryLogs q $$ sinkList

        logH entry@(Log nm m) = liftIO $
            let fp = P.encodeString (logDir </> logNamePath nm)
            in do withLogLock lk nm $ do
                      createDirectoryIfMissing True $ P.encodeString logDir
                      BS.appendFile fp (showLogMsg m)
                  return entry


-- Testing ---------------------------------------------------------------------

instance Arbitrary LogMsg where
  arbitrary = LogMsg <$> arbitrary <*> arbitrary

instance Arbitrary LogText where
  arbitrary = mkLogText . T.pack <$> arbitrary

instance Arbitrary LogLevel where
  arbitrary = do
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
      [ QC.testProperty "Just == readLogMsg . showLogMsg" $
          \m → Just m == readLogMsg (showLogMsg m)
      , testCase "showLogMsg" $
          showLogMsg (LogMsg Info (mkLogText "foo"))   @?= "[Info] foo\n"
      , testCase "showLogMsg on empty message" $
          showLogMsg (LogMsg Error (mkLogText ""))     @?= "[Error]\n"
      , testCase "Log messages are trimmed" $
          showLogMsg (LogMsg Warn (mkLogText " foo ")) @?= "[Warn] foo\n"
      ]

  , QC.testProperty "lastN == (\n → reverse . take n . reverse)" $
       \n (l∷[Int]) → let len = n `mod` 100000
                      in reverse (take len $ reverse l) == lastN len l

  , testCase "LogLevel ordering" $
      Debug<Info && Info<Warn && Warn<Error @?= True
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
