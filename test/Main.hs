module Main (main) where

import qualified Data.Text.Lazy as Lazy
import System.Directory (findExecutable)

import Data.Conduit.Run

main :: IO ()
main = go $ \exe -> do
    let args = ["it", "works!"]

    putStrLn $ showCmd "runTransparent" exe args
    (o,e) <- runTransparent textOutput exe args
    putStrLn $ "stdout: " ++ show (Lazy.toChunks o)
    putStrLn $ "stderr: " ++ show (Lazy.toChunks e)
    putStrLn ""

    putStrLn $ showCmd "runOpaque" exe args
    (o',e') <- runOpaque textOutput exe args
    putStrLn $ "stdout: " ++ show (Lazy.toChunks o')
    putStrLn $ "stderr: " ++ show (Lazy.toChunks e')
    putStrLn ""
  where
    go :: (FilePath -> IO ()) -> IO ()
    go f = findExecutable "echo" >>= mapM_ f

    showCmd :: String -> FilePath -> [String] -> String
    showCmd func exe args = unwords $ [func, show exe, show args]
