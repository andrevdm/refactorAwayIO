{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude hiding (catch, throwIO)
import qualified Prelude -- for the show instance
import qualified Data.Text as Txt
import qualified System.Console.ANSI as Ansi
import           Control.Exception.Safe (catch, throwIO)

import qualified Step1.Storage1 as S1
import qualified Step1.Impl1 as I1
import qualified Step2.Storage2 as S2
import qualified Step2.Impl2 as I2
import qualified Step3.Impl3 as I3


main :: IO ()
main = do
  ------------------------------
  -- demo 1
  -----------------------------
  header "Demo1: in memory"
  opsMem1 <- S1.mkMemOps
  demo1 opsMem1 `catch` handler

  header "Demo1: use file"
  opsFile1 <- S1.mkFileOps "data.txt"
  demo1 opsFile1 `catch` handler
  ------------------------------

  
  ------------------------------
  -- demo 2
  -----------------------------
  header "Demo2: in memory"
  opsMem2 <- S2.mkMemOps
  demo2 opsMem2 `catch` handler

  header "Demo2: use file"
  opsFile2 <- S2.mkFileOps "data.txt"
  demo2 opsFile2 `catch` handler
  ------------------------------

  
  ------------------------------
  -- demo 3
  -----------------------------
  header "Demo3: in memory"
  opsMem3 <- S2.mkMemOps
  demo3 opsMem3 --no catch

  header "Demo3: use file"
  opsFile3 <- S2.mkFileOps "data.txt"
  demo3 opsFile3 -- no catch
  ------------------------------


demo1 :: I1.Operations -> IO ()
demo1 ops = do
  let jobs = [ I1.Job "j1" job1
             , I1.Job "j2" job2
             , I1.Job "j3" job3
             ]
  
  r <- I1.runPipeline ops "0" jobs
  putText r


demo2 :: I2.Operations IO -> IO ()
demo2 ops = do
  let jobs = [ I2.Job "j1" job1
             , I2.Job "j2" job2
             , I2.Job "j3" job3
             ]
  
  r <- I2.runPipeline ops "0" jobs
  putText r


demo3 :: I2.Operations IO -> IO ()
demo3 ops = do
  let jobs = [ I2.Job "j1" job1
             , I2.Job "j2" job2
             , I2.Job "j3" job3
             ]
  
  r <- I3.runPipeline (I3.mkOpsWrapper ops) "0" jobs
  case r of
    Right x -> putText $ "Success: " <> x
    Left e -> putText $ "Exception: " <> show e
  


---------------------------------
-- User defined jobs
---------------------------------
job1 :: Text -> IO Text
job1 v = do
  putText "in job1"
  pure $ "1:" <> v

job2 :: Text -> IO Text
job2 v = do
  putText "in job2"
  void . throwIO $ DemoException "oops"
  pure $ "2:" <> v

job3 :: Text -> IO Text
job3 v = do
  putText "in job3"
  pure $ "3:" <> v
---------------------------------


handler :: SomeException -> IO ()
handler e = do
  Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red]
  putText $ "Exception: " <> show e
  Ansi.setSGR [Ansi.Reset]

  
header :: Text -> IO ()
header h = do
  putText ""
  putText ""
  Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Cyan]
  putText "---------"
  putText $ " " <> h
  putText "---------"


newtype DemoException = DemoException Text

instance Show DemoException where
  show (DemoException s) = Txt.unpack s
  
instance Exception DemoException
