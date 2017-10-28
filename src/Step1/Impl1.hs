{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Step1.Impl1 ( Operations (..)
                   , runImpl
                   ) where

import Protolude


data Operations = Operations { opRead :: IO Text
                             , opWrite :: Text -> IO ()
                             }


runImpl :: Operations -> IO Text
runImpl ops = do
  opWrite ops "initial"
  opWrite ops "value"
  t <- opRead ops
  opWrite ops $ "updated: " <> t
  opRead ops
