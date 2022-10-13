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
distro = liftM (flip (=~) pattern . (!! 0) . filter (isPrefixOf "NAME=") . lines) $ readFile "/etc/os-release"
  where pattern = "(?!\")[A-z\\d\\s\\/]+(?=\")"

main :: IO ()
main = do
  putStrLn =<< liftM3 (\x y z -> x ++ "@" ++ y ++ "\n" ++ z) user hostname distro
