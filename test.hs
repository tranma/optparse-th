
{-# LANGUAGE TemplateHaskell #-}

import Options.Applicative
import TH

data Command = Create String | Delete Arg
  deriving (Show, Read)
data Arg     = Arg Int String
  deriving (Show, Read)

$(derive ''Command)

data S1
  = SA
  | SB [Int] Char
  | SC P1
  | SD S2
  deriving (Show, Read)

data P1 = P1 Int Char
  deriving (Show, Read)
data S3 = S3A Int Int | S3B Int Char
  deriving (Show, Read)
data S2 = R | G | B
  deriving (Show, Read)

$(derive ''S1)

main :: IO ()
main = do
  x <- execParser p_S1_toplevel
  print x

