{-# LANGUAGE BlockArguments #-}

module Chapter2 where

import Control.Monad (join)
import Data.Function
import Data.List (sortBy)
import System.IO

getPopularNamesRows :: IO [String]
getPopularNamesRows = do
  fHandle <- openFile "./popular-names.txt" ReadMode
  lines <$> hGetContents fHandle

p10 :: IO ()
p10 = do
  rows <- getPopularNamesRows
  print $ length rows

p11 :: IO ()
p11 = do
  rows <- map (map (\c -> if c == '\t' then ' ' else c)) <$> getPopularNamesRows
  mapM_ putStrLn rows

p12 :: IO ()
p12 = do
  rows <- getPopularNamesRows
  let (col1, col2) = foldr (\r (c1, c2) -> let d1 : d2 : columns = words r in (d1 : c1, d2 : c2)) ([], []) rows
  writeFile "col1.txt" (unlines col1)
  writeFile "col2.txt" (unlines col2)

p13 :: IO ()
p13 = do
  col1Rows <- fmap lines (openFile "col1.txt" ReadMode >>= hGetContents)
  col2Rows <- fmap lines (openFile "col2.txt" ReadMode >>= hGetContents)
  let m = zipWith (\c1 c2 -> c1 <> "\t" <> c2) col1Rows col2Rows
  writeFile "col1-col2-mearged.txt" (unlines m)

p14 :: IO ()
p14 = do
  putStrLn "input number"
  n <- read <$> getLine :: IO Int
  rows <- getPopularNamesRows
  mapM_ putStrLn (take n rows)

p15 :: IO ()
p15 = do
  putStrLn "input number"
  n <- read <$> getLine :: IO Int
  rows <- getPopularNamesRows
  mapM_ putStrLn (reverse (take n (reverse rows)))

p16 :: IO ()
p16 = do
  putStrLn "input number"
  n <- read <$> getLine :: IO Int
  rows <- getPopularNamesRows
  let chunks = flip fix (rows, []) \loop (rest, chs) -> if length rest < n then chs ++ [rest] else loop (drop n rest, chs ++ [take n rest])
  mapM_ (\(ch, i) -> writeFile ("tmp/popular-names-" <> show i <> ".txt") (unlines ch)) (zip chunks (iterate (1 +) 1))

p17 :: IO ()
p17 = do
  rows <- getPopularNamesRows
  let col1 = join $ map (take 1 . words) rows
  let un = foldr (\c acc -> if c `elem` acc then acc else c : acc) [] col1
  mapM_ putStrLn un

p18 :: IO ()
p18 = do
  rows <- sortBy (flip (\a b -> compare (words a !! 3) (words b !! 3))) <$> getPopularNamesRows
  mapM_ putStrLn rows
