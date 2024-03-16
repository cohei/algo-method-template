{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main (main) where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Data.Functor (void)
import Numeric.Natural (Natural)
import System.IO (hSetEncoding, isEOF, stdin, stdout, utf8)
import Text.ParserCombinators.ReadP qualified as P

--- ここまでライブラリの import

main :: IO ()
main = _ -- dialog solve

solve :: _
solve = _

--- ライブラリ

-- 正規表現

match :: P.ReadP a -> String -> Bool
match parser =
  not . null . P.readP_to_S parser'
  where
    parser' = P.between skipAny (skipAny >> P.eof) parser
    skipAny = P.skipMany P.get

matchExact :: P.ReadP a -> String -> Bool
matchExact parser =
  not . null . P.readP_to_S (parser >> P.eof)

-- 整数論

isPrime :: (Integral a) => a -> Bool
isPrime n = factors n == [1, n]

factors :: (Integral a) => a -> [a]
factors n =
  [divider | divider <- [1 .. n], divider `isFactorOf` n]

isFactorOf :: (Integral a) => a -> a -> Bool
n `isFactorOf` m = m `mod` n == 0

-- 組み合わせ

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

-- IO

withUtf8 :: IO a -> IO a
withUtf8 action = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  action

dialog :: (Get a, Put b) => (a -> b) -> IO ()
dialog f = put . f =<< get

dialog2 :: (Get a, Get b, Put c) => (a -> b -> c) -> IO ()
dialog2 f = put =<< liftA2 f get get

class Get a where
  get :: IO a
  default get :: (Read a) => IO a
  get = readLn

instance Get () where
  get = void getLine

instance Get Natural

instance Get Int

instance Get Char where
  get = getChar

instance {-# OVERLAPS #-} Get String where
  get = getLine

instance (Get a, Get b) => Get (a, b) where
  get = (,) <$> get <*> get

instance (Get a, Get b, Get c) => Get (a, b, c) where
  get = (,,) <$> get <*> get <*> get

instance (Get a, Get b, Get c, Get d) => Get (a, b, c, d) where
  get = (,,,) <$> get <*> get <*> get <*> get

instance (Get a) => Get [a] where
  get = do
    eof <- isEOF
    if eof
      then pure []
      else do
        x <- get
        (x :) <$> get

newtype WithLength a = W a

instance (Get a) => Get (WithLength [a]) where
  get = fmap W . flip replicateM get =<< get

newtype Inline a = I {unI :: a}

instance {-# OVERLAPS #-} (Read a) => Get (Inline (a, String)) where
  get = (\[a, b] -> I (read a, b)) . words <$> getLine

instance (Read a, Read b) => Get (Inline (a, b)) where
  get = (\[a, b] -> I (read a, read b)) . words <$> getLine

instance (Read a, Read b, Read c) => Get (Inline (a, b, c)) where
  get = (\[a, b, c] -> I (read a, read b, read c)) . words <$> getLine

instance (Read a, Read b, Read c, Read d) => Get (Inline (a, b, c, d)) where
  get = (\[a, b, c, d] -> I (read a, read b, read c, read d)) . words <$> getLine

instance (Read a) => Get (Inline [a]) where
  get = I . map read . words <$> getLine

instance (Read a) => Get (WithLength (Inline [a])) where
  get = do
    n <- get
    W . I . take n . unI <$> get

class Put a where
  put :: a -> IO ()
  default put :: (Show a) => a -> IO ()
  put = print

instance Put Bool where
  put b = putStrLn $ if b then "Yes" else "No"

instance Put Natural

instance Put Int

instance Put Double

instance Put (Maybe Natural) where
  put = put . show @Integer . maybe (-1) fromIntegral

instance {-# OVERLAPS #-} Put String where
  put = putStrLn

instance (Put a, Put b) => Put (a, b) where
  put (a, b) = put a <> put b

instance (Show a) => Put (Inline [a]) where
  put = putStrLn . unwords . map show . unI

instance (Put a) => Put [a] where
  put = mapM_ put

instance {-# OVERLAPS #-} Put [[String]] where
  put = mapM_ (putStrLn . unwords)

-- [Inline [a]] でできそう
-- instance {-# OVERLAPS #-} Show a => Put [[a]] where
--  put = mapM_ (putStrLn . unwords . map show)

instance (Put a, Put b) => Put (Either a b) where
  put = either put put
