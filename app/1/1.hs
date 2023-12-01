module Main where

main :: IO ()
main = readFileBS "./app/1/input.txt" >>= (print . lines . decodeUtf8)
