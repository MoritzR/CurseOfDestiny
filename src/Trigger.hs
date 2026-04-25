{-# LANGUAGE TemplateHaskell #-}

module Trigger where

import Control.Monad.Free.Class (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import DataTypes

$(makeFree ''TriggerInstruction)
