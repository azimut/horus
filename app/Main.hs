module Main where

import qualified Store as S
import qualified Win as W

main :: IO ()
main = do
  img <- W.loadScreenshoot
  S.save img "/home/sendai/test.png"
  putStrLn "Done!"
