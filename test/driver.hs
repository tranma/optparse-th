{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import qualified Data.List as L
import Control.Monad
import System.Exit
import System.Process
import System.Directory
import System.FilePath.Posix
import Control.Exception

main :: IO ()
main = bracket_ setup cleanup goAll
  where
    goAll = do
      things <- getDirectoryContents testSrc
      dirs   <- filterM isDir $ filter (/= "..") $ filter (/= ".") things
      notice $ "tests: " ++ L.unwords dirs
      mapM_ go dirs
    isDir x = doesDirectoryExist (testSrc </> x)

testSrc = "test/cases"
tmp     = "tmp"
hs      = "hs"
inExt   = "in"
outExt  = "out"

go :: String -> IO ()
go test = do
  noticeSep
  notice_ $ test ++ ": ... "
  cases <- enumerateCases (testSrc </> test)
  compile test
  mapM_ (run test) cases
  where enumerateCases dir = do
          things <- getDirectoryContents dir
          return $ map dropExtension $ filter (withExtension inExt) things
        withExtension ext s = takeExtension s == '.':ext

compile :: FilePath -> IO ()
compile name = do
  let source    = testSrc </> name </> name <.> hs
      exeoutput = tmp     </> name
  (code, _, err) <- readProcessWithExitCode
    "ghc" ["-hidir", tmp, "-odir" , tmp, "-o", exeoutput, source] ""
  case code of
    ExitSuccess   -> notice "TH generated, compiled ok."
    ExitFailure _ -> do
      notice "compilation failed!"
      notice err

run :: FilePath -> FilePath -> IO ()
run name test = do
  noticeDot
  notice_ $ concat ["case: ", test, " of ", name, ": ... "]
  let exeoutput  = tmp     </> name
      inFile     = testSrc </> name </> test <.> inExt
      expectFile = testSrc </> name </> test <.> outExt
  input          <- readFile inFile
  hasExpect      <- doesFileExist expectFile
  expect         <- if   hasExpect
                    then readFile expectFile
                    else return ""
  (_, output, err) <- readProcessWithExitCode exeoutput (lines input) ""
  if   trim output == expect
  then notice "OK"
  else do notice $ concat ["FAIL: expecting ", expect, ", got ", output]
          notice err
          exitFailure

setup :: IO ()
setup = do
  b <- doesDirectoryExist tmp
  when b $ error "tmp already exists in current directory"
  createDirectory tmp

cleanup :: IO ()
cleanup = removeDirectoryRecursive tmp

trim :: String -> String
trim = takeWhile (/='\n')

notice    = putStrLn
noticeSep = notice "----------"
notice_   = putStr
noticeDot = notice_ "# "
