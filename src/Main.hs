{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import TunTap
import Irc

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan)
import Control.Monad (liftM2)
import System.Environment (getArgs)

main :: IO ()
main = do
   chs <- liftM2 (,) newChan newChan
   [ip,netid] <- getArgs
   _ <- forkIO $ tapThread chs 1500 ip
   _ <- ircThread chs "irc.freenode.net" "##ipoirc-test" 1 netid
   return ()
