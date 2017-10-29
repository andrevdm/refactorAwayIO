{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  id <- foldlM runJob 0 jobs

  opLog ops $ "\nfinal job id = " <> show id
  opRead ops

  where
    runJob (id :: Int) (Job name fn) = do
      opLog ops $ "running job: " <> name

      prev <- opRead ops
      r <- fn prev
      opWrite ops r
      
      opLog ops $ "  = " <> r
      opLog ops "  ----"

      pure $ id + 1
