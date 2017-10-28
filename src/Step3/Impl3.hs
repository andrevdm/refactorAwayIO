{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Step3.Impl3 ( mkOpsWrapper 
                   , runPipeline
                   ) where

import           Protolude hiding (catch)
import qualified Control.Monad.Except as E
import           Control.Exception.Safe (MonadCatch, SomeException, catch)

import qualified Step2.Impl2 as I2


data OpsError = ErrRead Text
              | ErrWrite Text
              | ErrLogging Text
              | ErrRunning Text
              deriving (Show, Eq)

-- | Operations wrapper
data OperationsWrapper m = OperationsWrapper { opRead :: E.ExceptT OpsError m Text
                                             , opWrite :: Text -> E.ExceptT OpsError m ()
                                             , opLog :: Text -> E.ExceptT OpsError m ()
                                             , opRun :: (Text -> m Text) -> Text -> E.ExceptT OpsError m Text
                                             }

-- | Create a wrapper over ops
mkOpsWrapper :: (MonadCatch m) => I2.Operations m -> OperationsWrapper m
mkOpsWrapper o =
  OperationsWrapper { opRead = E.ExceptT ((Right <$> I2.opRead o) `catch` readError)
                    , opWrite = \t -> E.ExceptT ((Right <$> I2.opWrite o t) `catch` writeError)
                    , opLog = \t -> E.ExceptT ((Right <$> I2.opLog o t) `catch` logError)
                    , opRun = \fn t -> E.ExceptT ((Right <$> fn t) `catch` logError)
                    }
  where
    readError :: (Monad m) => SomeException -> m (Either OpsError b)
    readError e = 
      pure . Left . ErrRead $ "Error reading: " <> show e
    
    writeError :: (Monad m) => SomeException -> m (Either OpsError b)
    writeError e = 
      pure . Left . ErrWrite $ "Error writing: " <> show e
    
    logError :: (Monad m) => SomeException -> m (Either OpsError b)
    logError e = 
      pure . Left . ErrLogging $ "Error logging: " <> show e


-- | Implementation using the safe operations
-- | Not using IO and because the wrapper catches all sync exceptions
-- | This code does not need to deal with exceptions
runPipeline :: (Monad m) => OperationsWrapper m -> Text -> [I2.Job m] -> m (Either OpsError Text)
runPipeline ops init jobs = E.runExceptT $ do
  opWrite ops init
  traverse_ runJob jobs
  r <- opRead ops

  opLog ops ""
  opLog ops $ "final result = " <> r
  pure r

  where
    runJob (I2.Job name fn) = do
      opLog ops $ "running job: " <> name
      prev <- opRead ops
      r <- opRun ops fn prev -- don't just lift, use opRun
      opLog ops $ "  = " <> r
      opLog ops "  ----"
      pure r
