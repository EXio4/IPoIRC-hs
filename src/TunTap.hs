-----------------------------------------------------------------------------
--
-- Module      :  TunTap
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
--import Main

module TunTap (
    tapThread
) where

import Shared

import Control.Monad (forever)
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.Chan

import qualified Data.ByteString.Base64 as B64

import Data.Word

import qualified Tap as TP

toIP :: String -> Word32
toIP = toIP' "" 0
    where
        calc p n = (read p) * (256^n)
        toIP' :: String -> Int -> String -> Word32
        toIP' st n []       = calc st n
        toIP' st n ('.':xs) = (toIP' "" (n+1) xs) + calc st n
        toIP' st n (x:xs)   = (toIP' (st ++ [x]) n xs)


-- withTAP from Network.TUNTAP source code modified a little bit
tapThread :: Chans -> Int -> String -> IO ()
tapThread (c1, c2) mtu ip =
    TP.start >>= \tap ->
    TP.openTAP tap "irc%d" >>
    TP.setMTU tap mtu >>
    TP.bringUp tap >>
    TP.setIP tap (toIP ip) >>
    forkIO (tunReader c1 tap) >>
    forkIO (tunWriter c2 tap) >>
    forever (threadDelay 1000000) >>
    TP.closeTAP tap >>
    TP.finish tap >>
    return ()

tunReader :: (Chan TunData) -> TP.TAP -> IO ()
tunReader ch t = forever $ do
                    x <- TP.readTAP t
                    putStrLn $ "tunReader :: " ++ (show (B64.encode x))
                    writeChan ch (CData x)

tunWriter :: (Chan IrcData) -> TP.TAP -> IO ()
tunWriter ch t = forever $ do
                    (CData x) <- readChan ch
                    putStrLn $ "tunWriter :: " ++ (show (B64.encode x))
                    TP.writeTAP t x
