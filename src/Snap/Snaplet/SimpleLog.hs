{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Snap.Snaplet.SimpleLog (
  SimpleLog(..),
  simpleLogInit,
  
  module System.Log
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Lens.Template
import Snap.Core
import Snap.Snaplet
import System.Log

data SimpleLog = SimpleLog {
  simpleLog :: Log }

simpleLogInit :: Politics -> Rules -> [IO Logger] -> SnapletInit b SimpleLog
simpleLogInit p rs ls = makeSnaplet "log" "Simple log" Nothing $ do
  l <- liftIO $ newLog p rs ls
  return $ SimpleLog l

instance MonadLog (Handler b SimpleLog) where
  askLog = gets simpleLog
