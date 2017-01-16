module Main where

import SatchSalvage.Extract

import Database.MySQL.Simple
import Options.Applicative
import Control.Applicative (optional)

-- Main Application Logic
main' :: CmdArgs -> IO ()
main' (CmdArgs d s) = do
  makeSkeleton
  conn <- connect defaultConnectInfo { connectDatabase = d }
  prods <- products conn
  mapM_ makeProductFs prods
  mapM_ (\dir -> mapM_ (copyImages dir conn) prods) s
  cats <- categories conn        
  mapM_ (makeCategoryFs conn) cats

-- Command line argument parsing
data CmdArgs = CmdArgs { db :: String
                       , staticDir :: Maybe String }

cmdargs :: Parser CmdArgs
cmdargs = CmdArgs
  <$> argument str (metavar "DATABASE" <> help "MySQL Database to extract from")
  <*> optional ( strOption ( long "dir" <> short 'd' <> metavar "DIRECTORY"
     <> help "Path to static directory in satchmo to extract images from" ))

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> cmdargs)
      ( fullDesc
     <> progDesc (unlines [
                     "Rescues data from a satchmo db into the filesystem."
                     , "",
                       "Optionally also fetches images from the static directory."
                     ])
     <> header "satchsalvage - rescue data from a satchmo database" )
