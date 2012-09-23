{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Snap.Snaplet.SimpleLog (
  SimpleLog(..),
  simpleLogInit, simpleLogInit_,
  
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

simpleLogInit :: RulesLoad -> [IO Logger] -> SnapletInit b SimpleLog
simpleLogInit rs ls = makeSnaplet "log" "Simple log" Nothing $ do
  l <- liftIO $ newLog rs ls
  return $ SimpleLog l

simpleLogInit_ :: Log -> SnapletInit b SimpleLog
simpleLogInit_ l = makeSnaplet "log" "Simple log" Nothing $ do
    return $ SimpleLog l

instance MonadLog (Handler b SimpleLog) where
  askLog = gets simpleLog
