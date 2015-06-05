{-# LANGUAGE TemplateHaskell #-}

import Data.Monoid
import Options.Applicative
import Options.Applicative.Types
import Options.Applicative.TH

data Action = CmdPing Int | CmdPong Int String

readString :: ReadM String
readString = readerAsk

$(genOptsRead ''Action [(''String, 'readString)])

main = do
  x <- execParser optparseAction
  case x of
    CmdPing x   -> putStrLn $ "ping: " ++ show x
    CmdPong y z -> putStrLn $ "pong: " ++ show y ++ " " ++ show z
