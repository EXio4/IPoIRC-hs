-----------------------------------------------------------------------------
--
-- Module      :  Protocol
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

module Protocol (
    genStr,
    parsemsg,
    Continue(..)
) where

import Data.List.Split (chunksOf)

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base64 as B64



data Continue = More
              | Done
              | NoMatch


genStr :: String -> C8.ByteString -> [C8.ByteString]
genStr nid = map C8.pack . process . chunksOf 128 . C8.unpack . B64.encode
      where
            process :: [String] -> [String]
            process []     = []
            process (x:[]) = [(nid ++ ":" ++ x)]
            process (x:xs) =  (nid ++ "<" ++ x):(process xs)


parsemsg :: C8.ByteString -> C8.ByteString -> (Continue,  C8.ByteString)
parsemsg netid input | netid == nid = (NoMatch, str)
                     | otherwise    = (matches,  str)
    where matches | ':' `C8.elem` input = Done
                  | '<' `C8.elem` input = More
                  | otherwise      = NoMatch
          [nid,str] = case matches of
                        Done -> C8.split ':' input
                        More -> C8.split '<' input
                        NoMatch -> [C8.pack "",C8.pack ""] -- "fallback"
