-----------------------------------------------------------------------------
--
-- Module      :  Irc
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

module Irc (
    ircThread,
) where

import Shared
import Protocol

import Control.Monad (forever, forM_)
import Data.Maybe (fromJust)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System.Random (randomRIO)

import Network.SimpleIRC

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base64 as B64

import qualified Data.HashTable.IO as H

type HashTable = H.LinearHashTable C8.ByteString C8.ByteString



type Netid = String
type ThreadCount = Int
type NetworkAddress = String
type ChannelName = String



ircWriter :: IrcConfig -> MIrc -> String -> Chan (ComData Tun) -> IO ()
ircWriter cfg s nid ch = forever $ do
                              (CData x) <- readChan ch
                              mapM_ (sendMsg s (C8.pack (head (cChannels cfg)))) (genStr nid x)



updateInP :: HashTable -> C8.ByteString -> (C8.ByteString -> C8.ByteString) -> IO ()
updateInP ht element f = do
        lElem <- H.lookup ht element
        case lElem of
            Just mutelem -> (H.insert ht element $ f mutelem)
            Nothing      -> (H.insert ht element $ f (C8.pack ""))
        Just z <- (H.lookup ht element)
        putStrLn $ "buffer updated " ++ (show z)
        return ()

evFPrivmsg :: HashTable -> Chan IrcData -> Netid -> EventFunc
evFPrivmsg ht ch nid _ msg = do
        let (st, str) = parsemsg (C8.pack nid) m
        case st of
            NoMatch -> return ()
            More -> updateInP ht nick (\x -> x `C8.append` str)
            Done -> do
                     lElem <- H.lookup ht nick
                     case lElem of
                        Just element -> H.insert ht nick (element `C8.append` str)
                        Nothing   -> H.insert ht nick str
                     Just z <- (H.lookup ht nick)
                     putStrLn $ "hi buffer: " ++ (show z)
                     case B64.decode z of
                        Left str' -> putStrLn $ "error ::: " ++ str' ++ " ::: in base64m decoding"
                        Right z'  -> writeChan ch $ CData z'
                     H.insert ht nick (C8.pack "")
                     return ()
        where
          nick = fromJust $ mOrigin msg
          m    = mMsg msg


randNick :: String -> IO String
randNick str = do
        num <- randomRIO (1,10000) :: IO Int
        return $ str ++ (show num)

genEv :: (EventFunc) -> [IrcEvent]
genEv func = [(Privmsg (func))]

genConfig :: String -> String -> [IrcEvent] -> IO (IrcConfig)
genConfig net chan ev = do
            rnick <- randNick "ipoirc"
            return ((mkDefaultConfig net rnick)
              { cChannels = [chan]
              , cEvents   = ev
              })

sndClient :: Chans -> Netid -> NetworkAddress -> ChannelName -> Int -> IO ()
sndClient (ch, _) nid network channame _ = do
    cfg <- (genConfig network channame [])
    (Right nhandle) <- connect cfg True False
    _ <- ircWriter cfg nhandle nid ch
    -- ??
    return ()

recClient :: Chans -> Netid -> NetworkAddress -> ChannelName -> (Chan IrcData -> Netid -> EventFunc) -> IO ()
recClient (tunch, clch) nid network channame func = do
    cfg <- (genConfig network channame (genEv (func clch nid)))
    (Right nhandle) <- connect cfg True False
    _ <- ircWriter cfg nhandle nid tunch -- re-use "this" thread
    return ()

-- start n irc clients with those settings
ircThread :: Chans -> NetworkAddress -> ChannelName -> ThreadCount -> Netid -> IO ()
ircThread ch network channame t nid = do
        forM_ [1..(t-1)]
            (forkIO .
                (sndClient ch nid network channame))
        ht <- H.new
        recClient ch nid network channame (evFPrivmsg ht)
