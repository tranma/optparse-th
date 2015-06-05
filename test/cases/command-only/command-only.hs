{-# LANGUAGE TemplateHaskell #-}

import Data.Monoid
import Options.Applicative
import Options.Applicative.TH

data Action = CmdPing | CmdPong

$(genOpts ''Action)

main = do
  x <- execParser optparseAction
  case x of
    CmdPing -> putStrLn "ping!"
    CmdPong -> putStrLn "pong!"

{-
pAction = info (subparser ((command "ping" (info pCmdPing mempty)) <> (command "pong" (info pCmdPong mempty)))) mempty
pCmdPing = pure CmdPing
pCmdPong = pure CmdPong
-}
