{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Control.Monad.State
import Data.Maybe (fromJust)
import System.IO (Handle, isEOF, openFile, IOMode(WriteMode), hTell)
import Data.Char (ord)
import Data.Binary (encode)


main :: IO ()
main = do
	l <- openFile "index/lazyH" WriteMode
	w <- openFile "index/wordH" WriteMode
	p <- openFile "index/positionH" WriteMode
	void $ runStateT (buildIndex ("", "")) $ Positions l w p

type Worker a = StateT Positions IO a

data Positions = Positions {lazyH::Handle, wordH::Handle, positionH::Handle} deriving Show

buildIndex :: (BS.ByteString, BS.ByteString) -> Worker ()
buildIndex prevData = liftIO isEOF >>= \end -> if end then return () else do
	line <- liftIO BS.getLine
	let (word, pos) = BS.break isSpace line
	key <- checkWordAndKey prevData word
	writePos positionH . fst . fromJust . BS.readInt . BS.tail $ pos
	buildIndex (key, word)

checkWordAndKey :: (BS.ByteString, BS.ByteString) -> BS.ByteString -> Worker BS.ByteString
checkWordAndKey (lastKey, lastWord) word =
	if lastWord == word then return lastKey
		else do
			writeKey lastKey key
			writePos positionH (-1)
			writeWord word
			return key
	where
		key = BS.take 3 word

writeKey :: BS.ByteString -> BS.ByteString -> Worker ()
writeKey lastKey key = if lastKey == key then return ()
	else do
		replicateM_ ((hash key) - (hash lastKey)) $ writePos lazyH (-1)
		get >>= liftIO . hTell . lazyH >>= writePos lazyH . fromInteger

writePos :: (Positions -> Handle) -> Int -> Worker ()
writePos h p = do
	handle <- get >>= return . h
	liftIO . writeInt handle $ p

writeWord :: BS.ByteString -> Worker ()
writeWord word = do
	handle <- get >>= return . wordH
	liftIO $ BS.hPut handle word
	get >>= \st -> liftIO $ hTell (positionH st) >>= writeInt handle . fromInteger

writeInt :: Handle -> Int -> IO ()
writeInt handle = LBS.hPut handle . encode

hash :: BS.ByteString -> Int
hash = sum . zipWith (*) [900, 30, 1] . map chash . BS.unpack
	where
		chash ' ' = 0
		chash 'ä' = (chash 'z') + 1
		chash 'å' = (chash 'z') + 2
		chash 'ö' = (chash 'z') + 3
		chash c = (ord c) - (ord 'a') + 1