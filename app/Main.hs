module Main where

import NLP.Chapter2 (p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
import System.Environment

data Chapter = Chapter2

data Command = RunAnswer Chapter Int

getChapter :: (Num a, Eq a) => a -> Chapter
getChapter n = case n of
  2 -> Chapter2
  _ -> undefined

getCommands :: IO Command
getCommands = do
  c : n : _ <- getArgs
  let chapter = getChapter (read c :: Int)
  let num = read n :: Int
  return $ RunAnswer chapter num

runCommand :: Command -> IO ()
runCommand s = case s of
  RunAnswer c n -> runAnswer c n

runAnswer :: Chapter -> Int -> IO ()
runAnswer c n = case c of
  Chapter2 -> case n of
    10 -> p10
    11 -> p11
    12 -> p12
    13 -> p13
    14 -> p14
    15 -> p15
    16 -> p16
    17 -> p17
    18 -> p18
    19 -> p19
    _ -> undefined

main :: IO ()
main = do
  command <- getCommands
  runCommand command