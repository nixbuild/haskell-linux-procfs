{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module System.Linux.Procfs
  ( ProcessStatus(..)
  , ProcessCPUStatus(..)
  , ProcessMemStatus(..)
  , SystemStat(..)
  , ClockTick
  , clockTick
  , processStatus
  , systemStat
  ) where

import "attoparsec" Data.Attoparsec.Text qualified as A
import "base" Control.Applicative
import "base" Data.Functor
import "base" Data.Word
import "base" Foreign.C.Types (CClock(..))
import "base" GHC.Generics
import "base" System.IO.Error
import "base" System.Posix.Types
import "binary" Data.Binary
import "text" Data.Text.IO qualified as T
import "text" Data.Text (Text)
import "time" Data.Time.Clock.POSIX (getPOSIXTime)
import "unix" System.Posix.Unistd (getSysVar, SysVar(ClockTick))

data ProcessStatus = ProcessStatus
  { timestamp :: !Double
  , memStat :: !ProcessMemStatus
  , cpuStat :: !ProcessCPUStatus
  } deriving (Show, Generic)

instance Binary ProcessStatus

data ProcessCPUStatus = ProcessCPUStatus
  { csUTime :: !Double
  , csSTime :: !Double
  , csGuestTime :: !Double
  } deriving (Show, Generic)

instance Binary ProcessCPUStatus

data ProcessMemStatus = ProcessMemStatus
  { msVmHWM :: !Word32
  , msRssAnon :: !Word32
  , msRssFile :: !Word32
  , msRssShmem :: !Word32
  } deriving (Show, Generic)

instance Binary ProcessMemStatus

data SystemStat = SystemStat
  { timestamp :: !Double
  , cpuUserTime :: !Double
  , cpuSystemTime :: !Double
  , cpuGuestTime :: !Double
  , processesCreated :: !Word32
  , memTotal :: !Word32
  , memCached :: !Word32
  , memTmpfs :: !Word32
  , memAvailable :: !Word32
  }
  deriving (Show, Generic)

instance Binary SystemStat

type PID = Int


clockTick :: IO ClockTick
clockTick = (CClock . fromIntegral) <$> getSysVar ClockTick

processStatus :: ClockTick -> PID -> IO (Maybe ProcessStatus)
processStatus ct pid = do
  timestamp <- realToFrac <$> getPOSIXTime
  parseFile (parseProcessCPUStatus ct) ("/proc/" ++ show pid ++ "/stat") >>= \case
    Nothing -> pure Nothing
    Just cpuStat ->
      parseFile parseProcessMemStatus ("/proc/" ++ show pid ++ "/status") >>= \case
        Nothing -> pure Nothing
        Just memStat -> pure $ Just ProcessStatus{..}

systemStat :: ClockTick -> IO (Maybe SystemStat)
systemStat ct = do
  timestamp <- realToFrac <$> getPOSIXTime
  parseFile (parseSystemKernelStat ct) "/proc/stat" >>= \case
    Nothing -> pure Nothing
    Just (cpuUserTime,cpuSystemTime,cpuGuestTime,processesCreated) ->
      parseFile parseSystemMemStat "/proc/meminfo" <&> \case
        Nothing -> Nothing
        Just (memTotal,memCached,memTmpfs,memAvailable) -> Just SystemStat{..}


-- Parsers

keyVal :: Char -> (Text -> Bool) -> A.Parser Text
keyVal d f = do
  key <- A.takeWhile (/= d) <* A.char d
  if not (f key) then fail "" else A.takeWhile (/= '\n') <* A.char '\n'

kilobyte :: Text -> A.Parser Word32
kilobyte f =
  A.skipMany (keyVal ':' (/= f)) *>
  A.string (f <> ":") *>
  many A.space *>
  A.decimal <*
  A.string " kB" <*
  A.char '\n'

parseProcessMemStatus :: A.Parser ProcessMemStatus
parseProcessMemStatus = do
  msVmHWM <- kilobyte "VmHWM"
  msRssAnon <- kilobyte "RssAnon"
  msRssFile <- kilobyte "RssFile"
  msRssShmem <- kilobyte "RssShmem"
  pure ProcessMemStatus{..}

parseSystemMemStat :: A.Parser (Word32,Word32,Word32,Word32)
parseSystemMemStat = do
  memTotal <- kilobyte "MemTotal"
  memAvailable <- kilobyte "MemAvailable"
  memBuffers <- kilobyte "Buffers"
  memCached <- kilobyte "Cached"
  memTmpfs <- kilobyte "Shmem"
  pure (memTotal,memBuffers+memCached,memTmpfs,memAvailable)

parseProcessCPUStatus :: ClockTick -> A.Parser ProcessCPUStatus
parseProcessCPUStatus ct = do
  -- pid
  _ <- int
  -- name
  _ <- field $ A.char '(' *> A.takeTill (== ')') <* A.anyChar
  -- state
  _ <- field A.anyChar
  -- ppid
  _ <- int
  -- gid
  _ <- int
  -- sid
  _ <- int
  -- tty
  _ <- int
  -- ttyGid
  _ <- int
  -- flags
  _ <- int
  -- minFlt
  _ <- word
  -- cMinFlt
  _ <- word
  -- majFlt
  _ <- word
  -- cMajFlt
  _ <- word
  -- uTime
  csUTime <- divTick ct <$> word
  -- sTime
  csSTime <- divTick ct <$> word
  -- cuTime
  _ <- int
  -- csTime
  _ <- int
  -- pri
  _ <- int
  -- nice
  _ <- int
  -- nt
  _ <- int
  -- '0'
  _ <- field $ A.char '0'
  -- st
  _ <- word64
  -- vSize
  _ <- word
  -- rss
  _ <- word
  -- rssLim
  _ <- word
  -- startCode
  _ <- word
  -- endCode
  _ <- word
  -- startStack
  _ <- word
  -- esp
  _ <- word
  -- eip
  _ <- word
  -- signal
  _ <- word
  -- blockedS
  _ <- word
  -- sigIgnore
  _ <- word
  -- sigCatch
  _ <- word
  -- wChan
  _ <- word
  -- nSwap
  _ <- word
  -- cnSwap
  _ <- word
  -- exitSignal
  _ <- int
  -- cpuNum
  _ <- int
  -- rtPri
  _ <- word
  -- policy
  _ <- word
  -- blkIOTicks
  _ <- word64
  -- guestTime
  csGuestTime <- divTick ct <$> word
  -- cGuestTime
  _ :: Word <- A.signed A.decimal
  pure $ ProcessCPUStatus{..}

parseSystemKernelStat :: ClockTick -> A.Parser (Double,Double,Double,Word32)
parseSystemKernelStat ct = do
  _ <- A.string "cpu"
  -- user
  cpuUserTime <- divTick ct <$> word64
  -- nice
  _ <- word64
  -- system
  cpuSystemTime <- divTick ct <$> word64
  -- idle
  _ <- word64
  -- iowait
  _ <- word64
  -- irq
  _ <- word64
  --softirq
  _ <- word64
  -- steal
  _ <- word64
  -- guest
  cpuGuestTime <- divTick ct <$> word64
  -- guest_nice
  _ <- word64
  A.skipMany $ keyVal ' ' (/= "processes")
  _ <- A.string "processes"
  processesCreated <- word32
  pure (cpuUserTime, cpuSystemTime, cpuGuestTime, processesCreated)

field :: A.Parser a -> A.Parser a
field p = many A.space *> p <* many A.space

word :: A.Parser Word
word = field A.decimal

word64 :: A.Parser Word64
word64 = field A.decimal

word32 :: A.Parser Word32
word32 = field A.decimal

int :: A.Parser Int
int = A.signed (field A.decimal)


-- Utils

divTick :: Integral a => ClockTick -> a -> Double
divTick (CClock ct) x = fromIntegral x / fromIntegral ct

readFileMaybe :: FilePath -> IO (Maybe Text)
readFileMaybe f = (Just <$> T.readFile f) `catchIOError` (\ _ -> pure Nothing)

parseFile :: A.Parser a -> FilePath -> IO (Maybe a)
parseFile p f = (>>= (A.maybeResult . A.parse p)) <$> readFileMaybe f
