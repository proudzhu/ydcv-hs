module ArgParse  where

import Options.Applicative

data Args = Args
  { full :: Maybe Bool
  , simple :: Maybe Bool
  , speech :: Maybe Bool
  , selection :: Maybe Bool
  , wordList :: [String]
  } deriving Show

argParse :: Parser Args
argParse = Args
   <$> optional (switch
       ( long "full"
      <> short 'f'
      <> help "print full web reference" ))
   <*> optional (switch
       ( long "simple"
      <> short 's'
      <> help "only show explainations" ))
   <*> optional (switch
      ( long "speech"
     <> short 'S'
     <> help "print URL to speech audio" ))
   <*> optional (switch
      ( long "selection"
     <> short 'X'
     <> help "show explainations of current selection" ))
   <*> some (argument str
      ( metavar "WORD" ))

{--
main :: IO ()
main = do
    conf <- execParser (info argParse fullDesc)
    print conf
--}
