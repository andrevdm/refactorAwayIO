{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module PipelineSpec where

import           Protolude 
import           Test.Hspec
import qualified Control.Monad.Except as E
import qualified Control.Monad.State.Strict as S

import qualified Step2.Impl2 as I2
import qualified Step3.Impl3 as I3

spec :: Spec
spec = do
  describe "simple pipeline" $ do
    it "should run in correct order" $ do
      let jobs = [ I2.Job "j1" job1
                 , I2.Job "j2" job2
                 ]
      
      let (r,_) = S.runState (testPipeline jobs "0") ""
      r `shouldBe` (Right "2:1:0")



testPipeline :: [I2.Job (S.State Text)] -> Text -> S.State Text (Either I3.OpsError Text)
testPipeline jobs initial = do
  let ops = I3.OperationsWrapper { I3.opRead = E.ExceptT $ do
                                     r <- get
                                     pure . Right $ r

                                 , I3.opWrite = \t -> E.ExceptT $ do
                                     put $ t
                                     pure . Right $ ()

                                 , I3.opRun = \fn t -> E.ExceptT $ do
                                     r <- fn t
                                     pure . Right $ r

                                 , I3.opLog = \t -> E.ExceptT . pure . Right $ ()
                                 }

  I3.runPipeline ops initial jobs


job1 :: Text -> (S.State Text) Text
job1 v =
  pure $ "1:" <> v

job2 :: Text -> (S.State Text) Text
job2 v = 
  pure $ "2:" <> v
