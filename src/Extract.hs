{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Codec.Compression.Zlib
import           Control.Applicative
import           Data.ByteString.Lazy    ( ByteString )
import           Data.Data
import           Data.Maybe
import           Data.RAF.Combined
import           Data.Typeable
import           System.Console.CmdArgs
import           System.Exit
import           System.FilePath
import           System.IO

import qualified Data.ByteString.Lazy as B
import qualified Data.Map             as M

data Extract = Extract
    { rafFile :: FilePath
    , datFile :: Maybe FilePath
    , targets :: [FilePath]
    } deriving (Show, Data, Typeable)

main :: IO ()
main = do
    opts <- cmdArgs program
    let fn  = rafFile opts
        fn' = fromMaybe (fn ++ ".dat") (datFile opts)
    raf  <- parseFromFile fn fn'
    case raf of
        Left e    -> do
            hPutStrLn stderr $ fn   ++ ":"
            hPutStrLn stderr $ fn'  ++ ":"
            hPutStrLn stderr $ "  " ++ e
            exitWith (ExitFailure 1)
        Right raf -> do
            isFine <- and <$> mapM (extract opts raf) (targets opts)
            if isFine then
                exitWith ExitSuccess
              else
                exitWith (ExitFailure 1)
  where
    program = Extract { rafFile = def &= argPos 0
                                      &= typFile
                      , datFile = def &= name "data"
                                      &= explicit
                                      &= typFile
                                      &= help ("Explicitly specify the"
                                           ++ " .RAF.DAT file; defaults to "
                                           ++ "FILE.dat.")
                      , targets = def &= args
                                      &= typ "TARGETS"
                      }

extract :: Extract -> RAF -> FilePath -> IO Bool
extract opts raf fn = maybe bail write (M.lookup fn raf)
  where
    bail = do
        hPutStrLn stderr $ fn ++ " not found"
        return False

    write bs = do
        B.writeFile (takeFileName fn) (decompress bs)
        return True

