{-# LANGUAGE TemplateHaskell #-}

import Data.Monoid
import Options.Applicative
import Options.Applicative.TH

newtype Number = Number Int
  deriving (Show, Read)

$(genOpts ''Number)

main :: IO ()
main = do
  x <- execParser optparseNumber
  print x
