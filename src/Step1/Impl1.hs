{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Step1.Impl1 ( Operations (..)
                   , Job (..)
                   , runPipeline
                   ) where

import Protolude


data Operations = Operations { opRead :: IO Text
                             , opWrite :: Text -> IO ()
                             }


data Job = Job { jobName :: Text
               , jobFn :: Text -> IO Text
               }

runPipeline :: Operations -> Text -> [Job] -> IO Text
runPipeline ops init jobs = do
  opWrite ops init
  id <- foldlM runJob 0 jobs

  putText $ "\nfinal job id = " <> show id
  opRead ops

  where
    runJob (id :: Int) (Job name fn) = do
      putText $ "running job: " <> name

      prev <- opRead ops
      r <- fn prev
      opWrite ops r
      
      putText $ "  = " <> r
      putText "  ----"

      pure $ id + 1
