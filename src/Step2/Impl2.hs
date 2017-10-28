{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Step2.Impl2 ( Operations (..)
                   , runImpl
                   ) where

import Protolude


data Operations m = Operations { opRead :: m Text
                               , opWrite :: Text -> m ()
                               }


runImpl :: (Monad m) => Operations m -> m Text
runImpl ops = do
  opWrite ops "initial"
  opWrite ops "value"
  t <- opRead ops
  opWrite ops $ "updated: " <> t
  opRead ops
