
{-# LANGUAGE TemplateHaskell #-}

import Options.Applicative
import TH

newtype MyAwesomeArgument a = MyAwesomeArgument a
  deriving (Show, Read)

data MyFabulousCommands = Foo (MyAwesomeArgument Int) | Bar Bool
  deriving (Show, Read)

$(deriveArgument ''MyAwesomeArgument)
$(deriveCommands ''MyFabulousCommands)
