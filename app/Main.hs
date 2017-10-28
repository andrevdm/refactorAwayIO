{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude hiding (catch)
import qualified System.Console.ANSI as Ansi
import           Control.Exception.Safe (catch)

import qualified Step1.Storage1 as S1
import qualified Step1.Impl1 as I1
import qualified Step2.Storage2 as S2
import qualified Step2.Impl2 as I2
import qualified Step3.Impl3 as I3


step1 :: IO ()
step1 = do
  runDemo "1" $ do
    opsMem <- S1.mkMemOps
    resultMem <- I1.runImpl opsMem
    putText $ "in memory result:\n   " <> resultMem

  runDemo "2" $ do
    opsFile <- S1.mkFileOps "data.txt"
    resultFile <- I1.runImpl opsFile
    putText $ "file result:\n   " <> resultFile
  
  runDemo "3" $ do
    opsFileErr <- S1.mkFileOps "\0"
    resultFileErr <- I1.runImpl opsFileErr
    putText $ "file (err) result:\n   " <> resultFileErr


step2 :: IO ()
step2 = do
  runDemo "4" $ do
    opsMem <- S2.mkMemOps
    resultMem <- I2.runImpl opsMem
    putText $ "in memory result:\n   " <> resultMem

  runDemo "5" $ do
    opsFile <- S2.mkFileOps "data.txt"
    resultFile <- I2.runImpl opsFile
    putText $ "file result:\n   " <> resultFile
  
  runDemo "6" $ do
    opsFileErr <- S2.mkFileOps "\0"
    resultFileErr <- I2.runImpl opsFileErr
    putText $ "file (err) result:\n   " <> resultFileErr


step3 :: IO ()
step3 = do
  runDemo "7" $ do
    opsMem <- S2.mkMemOps
    resultMem <- I3.runImplSafe $ I3.mkOpsWrapper opsMem
    case resultMem of
      Right r -> putText $ "in memory result:\n   " <> r
      Left e -> putText $ "in memory error:\n   " <> show e

  runDemo "8" $ do
    opsFile <- S2.mkFileOps "data.txt"
    resultFile <- I3.runImplSafe $ I3.mkOpsWrapper opsFile
    case resultFile of
      Right r -> putText $ "file result:\n   " <> r
      Left e -> putText $ "file result:\n   " <> show e
  
  runDemo "9" $ do
    opsFileErr <- S2.mkFileOps "\0"
    resultFileErr <- I3.runImplSafe $ I3.mkOpsWrapper opsFileErr
    case resultFileErr of
      Right r -> putText $ "file (err) result:\n   " <> r
      Left e -> putText $ "file (err) error:\n   " <> show e


main :: IO ()
main = do
  runStep "1" step1 `catch` handler
  runStep "2" step2 `catch` handler
  runStep "3" step3

  putText "done"


handler :: SomeException -> IO ()
handler e = do
  Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red]
  putText $ "Exception: " <> show e
  Ansi.setSGR [Ansi.Reset]


runDemo :: Text -> IO () -> IO ()
runDemo name demo = do
  Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Cyan]
  putText " ---------"
  putText $ "  DEMO " <> name
  putText " ---------"
  Ansi.setSGR [Ansi.Reset]
  demo
  Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Cyan]
  putText " --------\n"


runStep :: Text -> IO () -> IO ()
runStep name step = do
  Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Yellow]
  putText "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
  putText $ " STEP " <> name
  putText "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
  Ansi.setSGR [Ansi.Reset]
  step
  Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Yellow]
  putText "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n\n"
  Ansi.setSGR [Ansi.Reset]
