Template for `optparse-applicative`
===================================

There is no such thing as an options library I want to use. The only options parsing code I want to write is none at all! `(ノಠ益ಠ)ノ彡┻━┻`

This library should cover the basic cases of options parsing that I personally encounter the most:

 - `thing` some args
 - `thing` command args
 - `thing` command
 - `thing` args command args
 - ...
 
 You get the idea. (Er, see `Opt` in `TH.hs` for the actual idea.)
 
 An example:
 
 ```
 data Action = CmdPing | CmdPong

$(genOpts ''Action)

main = do
  x <- execParser optparseAction
  case x of
    CmdPing -> putStrLn "ping!"
    CmdPong -> putStrLn "pong!"
```

For more, plz check `test/cases`.

*TODO*

 * Help text: from Haddock. Unfortuntately the template library doesn't support docs. Gotta do something else.