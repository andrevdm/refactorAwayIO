{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Step2.Storage2 ( mkFileOps
                      , mkMemOps
                      ) where

import           Protolude
import qualified Data.Text.IO as Txt
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM (atomically)

import qualified Step2.Impl2 as I


--------------------------------------------------------
-- File based
--------------------------------------------------------
readFileOp :: FilePath -> IO Text
readFileOp = Txt.readFile


writeFileOp :: FilePath -> Text -> IO ()
writeFileOp = Txt.writeFile


mkFileOps :: FilePath -> IO (I.Operations IO) -- ^ Operations specialised to IO
mkFileOps p =
  pure I.Operations { I.opRead = readFileOp p
                    , I.opWrite = writeFileOp p
                    , I.opLog = putText
                    }
--------------------------------------------------------


--------------------------------------------------------
-- STM based
--------------------------------------------------------
readMemOp :: STM.TVar Text -> IO Text
readMemOp mem = do
  t <- atomically $ STM.readTVar mem
  pure t


writeMemOp :: STM.TVar Text -> Text -> IO ()
writeMemOp mem t = 
  atomically $ STM.writeTVar mem t


mkMemOps :: IO (I.Operations IO) -- ^ Operations specialised to IO
mkMemOps = do
  mem <- atomically $ STM.newTVar ""

  pure I.Operations { I.opRead = readMemOp mem
                    , I.opWrite = writeMemOp mem
                    , I.opLog = putText
                    }
--------------------------------------------------------
