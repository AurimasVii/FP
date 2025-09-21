{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Lib1 qualified
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )
import Control.Monad (forM_)

type Repl a = HaskelineT IO a

examples :: String
examples = "dump examples"

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

ini :: Repl ()
ini =
  liftIO $ putStrLn "Welcome! Press [TAB] for auto completion."

completer :: (Monad m) => WordCompleter m
completer _ = return [examples]

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd _  = liftIO $ forM_ (map show Lib1.examples) putStrLn

main :: IO ()
main =
  evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final
