{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Version (showVersion)
import Options.Applicative

import Paths_nestify (version)

import Nestify (nestify)


data CmdOptions = CmdOptions
  { _delim :: String }


(<$$>) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) = fmap . fmap


cmdOptions :: Parser CmdOptions
cmdOptions = CmdOptions
  <$> strOption
      ( long    "delimiter"
     <> short   'd'
     <> metavar "DELIM"
     <> value   ","
     <> showDefault
     <> help    "Use DELIM to delimit columns in the input" )


addVersion :: Parser (a -> a)
addVersion = infoOption ("nestify version " ++ showVersion version)
   ( long "version"
  <> help "Show version information" )


go :: CmdOptions -> IO ()
go (CmdOptions delim) = interact $ unlines . (intercalate delim <$>) . nestify . (splitOn delim <$>) . lines


main :: IO ()
main = execParser opts >>= go
  where
    opts = info (helper <*> addVersion <*> cmdOptions)
      ( fullDesc
     <> progDesc "Indent the last column using the penultimate column as a scope name" )
