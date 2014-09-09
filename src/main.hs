{-# LANGUAGE TupleSections, RankNTypes, OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Control.Monad.State
import Data.Maybe (fromJust)
import System.IO (Handle, isEOF, openFile, IOMode(WriteMode), hTell)
import Control.Monad.Identity (runIdentity, Identity(..))
import Control.Applicative (getConst, Const(..))
import Data.Char (ord)
import Data.Binary (encode)

main :: IO ()
main = do
	l <- openFile "index/lazyH" WriteMode
	w <- openFile "index/wordH" WriteMode
	p <- openFile "index/positionH" WriteMode
	void $ runStateT (buildIndex ("", "")) $ Positions l w p

type Worker a = StateT Positions IO a

data Positions = Positions {_lazyH::Handle, _word::Handle, _positionH::Handle} deriving Show

type Lens s a = Functor f => (a -> f a) -> s -> f s

lazyH :: Functor f => (Handle -> f Handle) -> Positions -> f Positions
lazyH f (Positions l w p) = fmap (\ll -> Positions ll w p) $ f l

wordH :: Functor f => (Handle -> f Handle) -> Positions -> f Positions
wordH f (Positions l w p) = fmap (\ww -> Positions l ww p) $ f w

positionH :: Functor f => (Handle -> f Handle) -> Positions -> f Positions
positionH f (Positions l w p) = fmap (\pp -> Positions l w pp) $ f p

_1 :: Functor f => (a -> f a) -> (a, b) -> f (a, b)
_1 f (x,y) = fmap (,y) $ f x

_2 :: Functor f => (a -> f a) -> (b, a) -> f (b, a)
_2 f (x,y) = fmap (x,) $ f y

over :: Lens s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f)

view :: Lens s a -> s -> a
view ln s = getConst $ ln Const s

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
		get >>= liftIO . hTell . view lazyH >>= writePos lazyH . fromInteger

writePos :: Lens Positions Handle -> Int -> Worker ()
writePos l p = do
	handle <- get >>= return . view l
	liftIO . writeInt handle $ p

writeWord :: BS.ByteString -> Worker ()
writeWord word = do
	handle <- get >>= return . view wordH
	liftIO $ BS.hPut handle word
	get >>= \st -> liftIO $ hTell (view positionH st) >>= writeInt handle . fromInteger

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