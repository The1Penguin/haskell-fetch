module Main where

import System.Environment ( getEnv )
import Network.HostName ( getHostName )
import Control.Monad ( liftM, ap )
import System.Info ( arch )
import System.Process ( readProcessWithExitCode )
import Data.Tuple.Select ( Sel2(sel2) )
import Text.Regex.PCRE ( (=~~), (=~) )
import Text.Printf ( printf )

user :: IO String
user = getEnv "USER"

hostname :: IO String
hostname = getHostName

distro :: IO String
distro = let
    pattern = "[A-z\\d\\s\\/]+(?=\\\")" :: String
    in
      flip (=~~) pattern =<< readFile "/etc/os-release"

architechture :: IO String
architechture = return arch

kernel :: IO String
kernel = liftM (head . drop 2 . words . sel2) $ readProcessWithExitCode "uname" ["-a"] ""

uptime :: IO String
uptime = let
  pattern = "(?<=up )[A-z\\s\\d,]+(?=\\n)" :: String
  in
    liftM (flip (=~) pattern . sel2) $ readProcessWithExitCode "uptime" ["-p"] ""

cpu :: IO String
cpu = let
  pattern = "(?<=\\s)[A-z\\s\\d]+(?=\\nstepping)" :: String
  in
    flip (=~~) pattern =<< readFile "/proc/cpuinfo"

display :: String -> String -> String -> String -> String -> String -> String -> String
display = printf "       __   %s@%s\n\
                 \-=(o '.     Distro:\t%s\n\
                 \   '.-.\\    Arch:\t%s\n\
                 \   /|  \\\\   Kernel:\t%s\n\
                 \   '|  ||   Uptime:\t%s\n\
                 \    _\\_):,_ CPU:\t%s"

main :: IO ()
main = putStrLn =<< return display `ap` user `ap` hostname `ap` distro `ap` architechture `ap` kernel `ap` uptime `ap` cpu
