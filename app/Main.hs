module Main where

import System.Environment
import Network.HostName
import Control.Monad
import System.IO
import Data.List
import Text.Regex.PCRE

user :: IO String
user = getEnv "USER"

hostname :: IO String
hostname = getHostName

distro :: IO String
distro = let
    distroPattern = "(?!\")[A-z\\d\\s\\/]+(?=\")" :: String
    linePattern = "^NAME=.*" :: String
    in
      liftM (flip (=~) distroPattern :: (String -> String)) $ liftM (flip (=~) linePattern :: (String -> String)) $ readFile "/etc/os-release"

main :: IO ()
main = do
  putStrLn =<< liftM3 (\x y z -> x ++ "@" ++ y ++ "\n" ++ z) user hostname distro
