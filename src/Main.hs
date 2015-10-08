module Main where

import System.Process
import System.Environment
import Data.List
import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as L


main :: IO ()
main = do
    args <- getArgs

    if not $ null args
        then do
            let mode:rest = args
            doCommand mode rest
        else system "ls -al" >>= \exitCode -> print exitCode

doCommand mode [owner, repo] | mode == "--issue" = doIssue owner repo
doCommand mode rest          | mode == "-git"    = system ("git " ++ unwords rest) >>= \exitCode -> print exitCode
doCommand _ _                                    = system "git status" >>= \exitCode -> print exitCode


doIssue owner repo = do
    res <- issue owner repo
    liftIO $ L.putStr $ responseBody res

issue owner repo = do
    manager <- liftIO $ newManager tlsManagerSettings
    req <- liftIO $ parseUrl $ "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/issues"
    let userAgent = ("User-Agent", "github-flux Mozzarella file box google ultron")
    let reqHead = req {
                        method = "GET"
                       ,requestHeaders = [userAgent]
                      }
    httpLbs reqHead manager
