{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Control.Monad
import           Data.ByteString          ( ByteString )
import           Data.Data
import           Data.List
import           Data.RAF
import           Data.Typeable
import           System.Console.CmdArgs
import           System.Exit
import           System.IO

import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as M

data List = List
    { archives :: [FilePath]
    } deriving (Show, Data, Typeable)

main :: IO ()
main = do
    opts <- cmdArgs list
    case archives opts of
        []   -> return ()
        [fn] -> run fn
        fns  -> forM_ fns $ \fn -> print fn >> run fn
  where
    list = List { archives = def &= args &= typFile }

run :: FilePath -> IO ()
run fn = do
    status <- parseFromFile fn
    case status of
        Left e -> do
            hPutStrLn stderr $ fn   ++ ":"
            hPutStrLn stderr $ "  " ++ e
            exitWith (ExitFailure 1)
        Right raf -> do
            mapM_ putStrLn $ sort $ M.keys raf
            exitWith ExitSuccess

