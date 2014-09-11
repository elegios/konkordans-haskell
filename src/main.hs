{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace, toLower)
import Control.Monad.State
import Data.Maybe (fromJust)
import System.IO (Handle, isEOF, openFile, IOMode(WriteMode, ReadMode), hTell, hSeek, SeekMode(AbsoluteSeek))
import Data.Char (ord)
import Data.Binary (decode, encode)
import System.Environment (getArgs)
import Data.Functor ((<$>))


main :: IO ()
main = do
	(word:_) <- getArgs
	if word == "build-index" then
		openFiles WriteMode >>= void . runStateT (buildIndex ("", ""))
	else do
		p <- openFiles ReadMode
		t <- openFile "korpus" ReadMode
		hSeek (lazyH p) AbsoluteSeek . toInteger . (*8) . hash $ BS.pack word
		wordPointer <- readInt $ lazyH p
		endPointer <- readInt $ lazyH p
		pointerPointer <- search (wordH p) wordPointer endPointer $ BS.pack word
		case pointerPointer of
			Nothing -> putStr $ "Hittade inte " ++ word
			Just a -> do
				hSeek (positionH p) AbsoluteSeek $ toInteger a
				occurrences <- printOccurrences (positionH p) t (length word)
				putStrLn $ "Det finns " ++ show (length occurrences) ++ " förekomster av ordet."
				if length occurrences > 25
					then do
						putStrLn "Oj! Det var många. Vill du printa dem?"
						res <- getLine
						case map toLower res of
							'y':_ -> sequence_ occurrences
							_ -> return ()
					else sequence_ occurrences

printOccurrences :: Handle -> Handle -> Int -> IO [IO ()]
printOccurrences positionHandle textHandle l = do
	p <- readInt positionHandle
	let postpos = - 30 - (min (p - 30) 0)
	if p == -1 then return [] else do
		hSeek textHandle AbsoluteSeek $ toInteger (p + postpos)
		text <- BS.hGet textHandle (30 - postpos + l)
		let printWord = putStrLn . BS.unpack . BS.map (\c -> if c == '\n' then ' ' else c) $ text
		let padding = replicateM_ (30 + postpos) (putStr " ")
		printOccurrences positionHandle textHandle l >>= return . ((padding >> printWord) :)

search :: Handle -> Int -> Int -> BS.ByteString -> IO (Maybe Int)
search index start end word = do
	firstWord <- BS.hGetLine index
	if firstWord > word
		then return Nothing
		else linsearch index start word

linsearch :: Handle -> Int -> BS.ByteString -> IO (Maybe Int)
linsearch index start word = do
	hSeek index AbsoluteSeek $ toInteger start
	linesearchRec
	where
		linesearchRec = do
			testWord <- BS.hGetLine index
			case compare testWord word of
				GT -> return Nothing 
				EQ -> Just <$> readInt index
				LT -> readInt index >> linesearchRec


{-binsearch :: Handle -> Int -> Int -> BS.ByteString -> IO (Maybe Int)
binsearch index start end word = do
	hSeek index AbsoluteSeek ((end - start)/2)
	BS.hGetLine index >> readInt index
	wordPos <- hTell -}

readInt :: Handle -> IO Int
readInt h = decode <$> LBS.hGet h 8

openFiles :: IOMode -> IO Positions
openFiles mode = do
	l <- openFile "index/lazyH" mode
	w <- openFile "index/wordH" mode
	p <- openFile "index/positionH" mode
	return $ Positions l w p

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
		wordPointer <- fromInteger <$> (get >>= liftIO . hTell . wordH)
		replicateM_ ((hash key) - (hash lastKey) + 1) $ writePos lazyH wordPointer

writePos :: (Positions -> Handle) -> Int -> Worker ()
writePos h p = do
	handle <- get >>= return . h
	liftIO . writeInt handle $ p

writeWord :: BS.ByteString -> Worker ()
writeWord word = do
	handle <- get >>= return . wordH
	liftIO $ BS.hPut handle word
	liftIO $ BS.hPut handle "\n"
	get >>= liftIO . hTell . positionH >>= liftIO . writeInt handle . fromInteger

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
