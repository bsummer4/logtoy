{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, LambdaCase       #-}
{-# LANGUAGE OverloadedStrings, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, TypeOperators, UnicodeSyntax                    #-}

{-
  # Concerns

  Current, logs files are read start-to-end even though the limit parameter
  yields the last `n` logs. This is a problem, since reads on long files take
  linear time wrt the file length. It should be possible to efficiently
  read the file backwards, and that's probably worth doing here.

  When using the `limit` parameter on a large log file, we need to keep all
  of the results in memory. This has abuse potential. Some solutions that
  are worth consideration:

    - Put a maximum bound on the `limit` parameter.
    - Archive old logs to keep the log files form becoming too large.
    - Return logs in reverse order, and also read the log file in reverse
      order. This would allow us to stream the logs in constant memory.

  Some IO exceptions are not handled, however warp seems to do the right
  in these situations. It returns a 500 error and prints a warning
  to stderr.

  Using `fsync()` on writes might be worth consideration. Do we care
  about losing some logs if the system crashes? I'm going to go with "no"
  for now.

  `servant` does not do much validation on the requests, but it would
  take a lot of code to do the validation myself. Specifically,

    - it does not validate the request method. It accepts requests from
      any method without complaint.
    - it ignores invalid query parameters.  For example, making a request
      to '/read/test?level=warnn' returns all of the logs in tests.
-}

module Main (main,test) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad
import Control.Monad.Base
import Control.Monad.Primitive
import Control.Monad.ST

import           Data.Aeson                       ((.:), (.=))
import qualified Data.Aeson                       as J
import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Builder          as BSB
import qualified Data.ByteString.Lazy             as LBS
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Format                 as T
import qualified Data.Text.Lazy                   as LT
import qualified Data.Vector                      as BV
import           Data.Vector.Generic              ((!))
import qualified Data.Vector.Generic              as V
import qualified Data.Vector.Generic.Mutable      as VM

import           Pipes
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude    as P
import qualified Pipes.Vector     as P
import qualified Pipes.Wai        as P

import qualified Network.HTTP.Types       as W
import qualified Network.Wai              as W
import qualified Network.Wai.Handler.Warp as W

import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as P
import           System.Directory
import           System.Environment
import           System.IO (withFile, IOMode(ReadMode))
import           System.IO.Error
import           System.Posix.Files

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Servant hiding (Proxy)
import qualified Servant as Serv
import Text.Printf


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

-- Rotate the elements of a vector left by `offset` elements.
rotateL ∷ (V.Vector v a) => Int → v a → v a
rotateL offset vec | (offset `mod` V.length vec) == 0 = vec
rotateL offset vec                                    =
  V.generate sz $ \i → vec ! mod (i + offset) sz
    where sz = V.length vec

-- Using an internal ring buffer, return a vector containing the last
-- `nElems` elements of a Pipe stream.
sinkVecLastN ∷ (MonadBase base m, V.Vector v a, PrimMonad base)
            => Int
             → Consumer a m (v a)
sinkVecLastN nElems | nElems <= 0 = return V.empty
sinkVecLastN nElems               = do
    firstN ← P.runToVectorP $ P.take nElems >-> P.toVector
    case compare (V.length firstN) nElems of
      LT → return firstN
      GT → error "Pipes.take returned too many elements!"
      EQ → do
          mv <- liftBase $ V.unsafeThaw firstN
          let go n = do
                  let i = n `mod` nElems
                  mx <- await
                  case mx of
                      Nothing -> rotateL i <$> liftBase (V.unsafeFreeze mv)
                      Just x  -> do liftBase $ VM.write mv i x
                                    go (n + 1)
          go 0

-- Yeild the final n elements of a stream.
lastNC ∷ (MonadBase base m, PrimMonad base) => Int → Pipe a a m ()
lastNC n = sinkVecLastN n >>= BV.mapM_ yield

-- The last n elements of a list.
lastN ∷ Int → [a] → [a]
lastN n xs = runST $ P.toListM $ each xs >-> lastNC n

fuseMaybe ∷ Monad m => Pipe a b m () → Maybe (Pipe b b m ()) → Pipe a b m ()
fuseMaybe source Nothing  = source
fuseMaybe source (Just c) = source >-> c

-- This is a modified version of Network.Wai.responseStream that runs
-- in resourceT.
-- responseProducer ∷ W.Status
               -- → W.ResponseHeaders
               -- → Producer (Consumer () (ResourceT IO)) (Flush BSB.Builder)
               -- → W.Response
responseProducer s hs src = undefined
  -- W.responseStream s hs $ \send flush ->
    -- runResourceT $ runEffect $
      -- src $$ P.mapM_ (\case Chunk b -> liftIO $ send b
                            -- Flush   -> liftIO flush)


-- Lines in Log Files ----------------------------------------------------------

-- attoparsec parser for log messages.
readLogMsg ∷ ByteString → Maybe LogMsg
readLogMsg bs = toMaybe $ flip A.parseOnly bs $ do
    let bsToText = T.decodeUtf8 . BS.pack
    _ ← A8.char '['
    Just l ← fromText . T.decodeUtf8 <$> A8.takeWhile A8.isAlpha_ascii
    _ ← A8.char ']'
    m ← mkLogText . bsToText <$> A.many' (A.notWord8 $ fromIntegral $ ord '\n')
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
  parseJSON = J.withText "log text" $ \s →
                return $ mkLogText s

instance J.FromJSON LogLevel where
  parseJSON = J.withText "log level" $ \s →
                fromMaybe (fail "Invalid log level") $
                  return <$> fromText s

instance J.FromJSON LogName where
  parseJSON = J.withText "log name" $ \s →
                fromMaybe (fail ("Invalid log name: " <> T.unpack s)) $
                 return <$> readLogName s

instance J.FromJSON Log where
  parseJSON = J.withObject "log" $ \v →
                Log <$> v .: "name"
                    <*> (LogMsg <$> v .: "level"
                                <*> v .: "msg")

instance J.ToJSON Log where
  toJSON (Log n (LogMsg level msg)) =
    J.object [ "name"  .= unLogName n
             , "level" .= show level
             , "msg"   .= unLogText msg
             ]


-- Log File IO -----------------------------------------------------------------

logFile ∷ P.FilePath → LogName → FilePath
logFile logDir nm = P.encodeString $ logDir </> logNamePath nm

-- Throws a permission error if we don't have read access to the file.
getLogFileSize ∷ (LogLock,P.FilePath) → LogName → IO Int
getLogFileSize (lk,ld) nm = do
    let fn = logFile ld nm
        readError =
          mkIOError permissionErrorType "getLogFileSize" Nothing (Just fn)
    withLogLock lk nm $ do
        haveAccess ← fileAccess fn True False False
        unless haveAccess (ioError readError)
        stat ← getFileStatus fn
        return $ fromIntegral $ fileSize stat

logLevelsGE ∷ Monad m => LogLevel → Pipe Log Log m ()
logLevelsGE minLevel = P.filter (\l → logLevel l >= minLevel)
  where logLevel (Log _ (LogMsg l _)) = l

queryLogs ∷ (MonadBase b m,PrimMonad b) => ReadQuery → Pipe ByteString Log m ()
queryLogs (ReadQuery nm levelMay limitMay) =
  PB.lines >-> P.mapFoldable (fmap (Log nm) . readLogMsg)
    `fuseMaybe` (logLevelsGE <$> levelMay)
    `fuseMaybe` (lastNC      <$> limitMay)


-- Server Stuff ----------------------------------------------------------------

type Api = "read" :> Capture "logname" LogName
                  :> QueryParam "limit" Int
                  :> QueryParam "level" LogLevel
                  :> Raw -- Get [Log]
      :<|> "log" :> ReqBody Log
                 :> Post Log

-- Converts a stream of JSON values into a JSON list, and streams a
-- serialized verison of that list.
streamJSONList ∷ (J.ToJSON j,MonadIO m) ⇒ Pipe j (P.Flush BSB.Builder) m ()
streamJSONList = go (0∷Integer) where
  go sent = do
      mx ← await
      when (sent == 0) (yield $ P.Chunk $ BSB.char7 '[')
      case mx of
        Nothing → yield $ P.Chunk $ BSB.char7 ']'
        Just j → do
            unless (sent == 0) (yield $ P.Chunk $ BSB.char7 ',')
            yield $ P.Chunk $ BSB.byteString $ LBS.toStrict $ J.encode j
            when (0 == (sent+1) `mod` 1000) (yield P.Flush)
            go (sent+1)

appendLog ∷ MonadIO m => (LogLock, P.FilePath) → Log → m Log
appendLog (lk,logDir) entry@(Log nm m) =
  liftIO $ withLogLock lk nm $ do
      createDirectoryIfMissing True $ P.encodeString logDir
      BS.appendFile (logFile logDir nm) (showLogMsg m)
      return entry

sourceFile ∷ FilePath → Producer' ByteString m ()
sourceFile fn = withFile fn ReadMode P.fromHandle

-- Here, we rely on the fact that our IO files are append-only to keep
-- the lock duraction very short. We use the lock only to grab the size of
-- the file, and then we stream the first n bytes of the file. This way,
-- even if the end of the file goes into an inconsistent state while we
-- are reading, this doesn't affect us.
--
-- HACK ALERT: If the file does not exist, we pretend that we are
-- reading from a file with size 0. If `sourceFile` is never asked
-- for any chunks, then the file will never be read. We rely on
-- this behavior to avoid getting further io errors from trying to
-- read the non-existent file.
getLogs ∷ (LogLock, P.FilePath)
        → LogName → Maybe Int → Maybe LogLevel
        → W.Application
getLogs (lk,logDir) nm limit level req respond =
  if W.requestMethod req /= "GET"
  then
    respond $ W.responseLBS W.status404 [] "Expecting a GET request"
  else
    do let fp = logFile logDir nm
       szE ← liftIO $ tryIOError $ getLogFileSize (lk,logDir) nm
       sz ← case szE of Left e | isDoesNotExistError e → return 0 -- See note.
                        Left e                         → liftIO (ioError e)
                        Right s                        → return (s∷Int)
       let q = ReadQuery nm level limit
       let logStream = sourceFile fp >-> PB.take sz >-> queryLogs q
       let jsonResp = ("Content-Type", "application/json")
       respond $ P.responseProducer W.status200 [jsonResp] $
           logStream >-> streamJSONList

apiServer ∷ (LogLock,P.FilePath) → W.Application
apiServer config = serve (Serv.Proxy∷Serv.Proxy Api) (readH :<|> logH)
  where logH  = appendLog config
        readH = getLogs config


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
  [ QC.testProperty "Just == readLogMsg . showLogMsg" $
      \m → Just m == readLogMsg (showLogMsg m)

  , testCase "showLogMsg" $
      showLogMsg (LogMsg Info (mkLogText "foo"))   @?= "[Info] foo\n"

  , testCase "showLogMsg on empty message" $
      showLogMsg (LogMsg Error (mkLogText ""))     @?= "[Error]\n"

  , testCase "Log messages are trimmed" $
      showLogMsg (LogMsg Warn (mkLogText " foo ")) @?= "[Warn] foo\n"

  , testCase "Log messages can't contain newlines" $
      showLogMsg (LogMsg Debug (mkLogText "fo\no")) @?= "[Debug] foo\n"

  , QC.testProperty "lastN == (\\n → reverse . take n . reverse)" $
       \len (l∷[Int]) → reverse (take len $ reverse l) == lastN len l

  , QC.testProperty "lastN handles absurd values for N" $
      \(l∷[Int]) → null(lastN minBound l) && lastN maxBound l == l

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
    deepseq logDir $ W.run port $ apiServer (lk,logDir)
