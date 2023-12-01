{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char (isAlpha)
import Data.Text (unpack)

main :: IO ()
main =
  readFileBS "./app/1/input.txt"
    >>= ( decodeUtf8
            >>> lines
            >>> fmap (unpack >>> filter (not . isAlpha) >>> nonEmpty)
            >>> sequence
            >>> fmap (fmap $ ((: []) . head) &&& ((: []) . last))
            >>> fmap (fmap $ uncurry (<>))
            >>> sequence
            . concatMap (fmap $ readMaybe @Int)
            >>> fmap sum
            >>> print
        )
