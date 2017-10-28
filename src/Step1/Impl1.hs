{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
  traverse_ runJob jobs
  r <- opRead ops

  putText ""
  putText $ "final result = " <> r
  pure r

  where
    runJob (Job name fn) = do
      putText $ "running job: " <> name
      prev <- opRead ops
      r <- fn prev
      putText $ "  = " <> r
      putText "  ----"
      pure r
