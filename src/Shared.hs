-----------------------------------------------------------------------------
--
-- Module      :  Shared
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE EmptyDataDecls #-}

module Shared where

import qualified Data.ByteString as BS
import Control.Concurrent.Chan

data ComData a = CData BS.ByteString
    deriving (Show, Eq)

data Tun
data Irc

type TunData = ComData Tun
type IrcData = ComData Irc

type Chans = (Chan TunData, Chan IrcData)
