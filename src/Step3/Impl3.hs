{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Step3.Impl3 ( mkOpsWrapper 
                   , runImplSafe
                   ) where

import           Protolude hiding (catch)
import qualified Control.Monad.Except as E
import           Control.Exception.Safe (MonadCatch, SomeException, catch)

import qualified Step2.Impl2 as I


data OpsError = ErrRead Text
              | ErrWrite Text
              deriving (Show, Eq)

-- | Operations wrapper
data OperationsWrapper m = OperationsWrapper { opRead :: E.ExceptT OpsError m Text
                                             , opWrite :: Text -> E.ExceptT OpsError m ()
                                             }

-- | Create a wrapper over ops
mkOpsWrapper :: (MonadCatch m) => I.Operations m -> OperationsWrapper m
mkOpsWrapper o =
  OperationsWrapper { opRead = E.ExceptT ((Right <$> I.opRead o) `catch` readError)
                    , opWrite = \t -> E.ExceptT ((Right <$> I.opWrite o t) `catch` writeError)
                    }
  where
    readError :: (Monad m) => SomeException -> m (Either OpsError b)
    readError e = 
      pure . Left . ErrRead $ "Error reading: " <> show e
    
    writeError :: (Monad m) => SomeException -> m (Either OpsError b)
    writeError e = 
      pure . Left . ErrWrite $ "Error writing: " <> show e


--------------------------------------------------------
-- Implementation using the operations
-- Not using IO, m is any monad. No IO allowed
--------------------------------------------------------
runImplSafe :: (Monad m) => OperationsWrapper m -> m (Either OpsError Text)
runImplSafe ops = E.runExceptT $ do
  opWrite ops "initial"
  opWrite ops "value"
  t <- opRead ops
  opWrite ops $ "updated: " <> t
  opRead ops
--------------------------------------------------------

