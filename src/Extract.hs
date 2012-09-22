{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Codec.Compression.Zlib
import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Lazy    ( ByteString )
import           Data.Data
import           Data.Maybe
import           Data.RAF.Combined
import           Data.Typeable
import           System.Console.CmdArgs
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO

import qualified Data.ByteString.Lazy as B
import qualified Data.Map             as M

data Extract = Extract
    { rafFile   :: FilePath
    , datFile   :: Maybe FilePath
    , targets   :: [FilePath]
    , baseName  :: Bool
    , changeDir :: Maybe FilePath
    } deriving (Show, Data, Typeable)

main :: IO ()
main = do
    opts <- cmdArgs program
    let fn  = rafFile opts
        fn' = fromMaybe (fn `addExtension` "dat") (datFile opts)
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
    program = Extract { rafFile   = def
                                  &= argPos 0
                                  &= typFile
                      , datFile   = def
                                  &= name "data"
                                  &= explicit
                                  &= typFile
                                  &= help ("Explicitly specify the "
                                        ++ ".RAF.DAT file; defaults to "
                                        ++ "FILE.dat.")
                      , targets   = def
                                  &= args
                                  &= typ "TARGETS"
                      , baseName  = False
                                  &= name "b"
                                  &= name "base"
                                  &= explicit
                                  &= help ("Extract file to basename instead "
                                        ++ "of full file path.")
                      , changeDir = def
                                  &= name "C"
                                  &= name "directory"
                                  &= explicit
                                  &= help ("Change into DIR before "
                                        ++ "extraction.")
                      }

extract :: Extract -> RAF -> FilePath -> IO Bool
extract opts raf trg = do
    createDirectoryIfMissing True cwd

    case M.lookup trg raf of
        Nothing -> do
            hPutStrLn stderr $ trg ++ " not found"
            return False
        Just bs -> do
            let (fp, fn) = splitFileName trg
                ofn | baseName opts = cwd </> fn
                    | otherwise     = cwd </> fp ++ fn
            unless (baseName opts) $ do
                createDirectoryIfMissing True $ cwd ++ fp
            B.writeFile ofn (decompress bs)
            return True
  where
    cwd = fromMaybe "." $ changeDir opts
