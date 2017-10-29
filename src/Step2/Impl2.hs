{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Step2.Impl2 ( Operations (..)
                   , Job (..)
                   , runPipeline
                   ) where

import Protolude


data Operations m = Operations { opRead :: m Text
                               , opWrite :: Text -> m ()
                               , opLog :: Text -> m ()
                               }


data Job m = Job { jobName :: Text
                 , jobFn :: Text -> m Text
                 }

runPipeline :: (Monad m) => Operations m -> Text -> [Job m] -> m Text
runPipeline ops init jobs = do
  opWrite ops init
  r <- foldlM runJob init jobs

  opLog ops ""
  opLog ops $ "final result = " <> r
  pure r

  where
    runJob prev (Job name fn) = do
      opLog ops $ "running job: " <> name
      r <- fn prev
      opLog ops $ "  = " <> r
      opLog ops "  ----"
      pure r
