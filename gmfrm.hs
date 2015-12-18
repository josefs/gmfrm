import System.Console.CmdArgs.Explicit
import System.Directory (doesFileExist)
import Data.Char (isDigit)

import Assemble
import Show

data Cmd = Show { help  :: Bool
                , files :: [String]
                , magnification :: Int
                }
         | Assemble { help  :: Bool
                    , files :: [String]
                    , out   :: String
                    }
         | Default { help :: Bool }

showMode = mode "show" (Show False [] 0) showHelp (flagArg upd "[input files]")
           [ flagOpt "magnify" ["m"] m "magnification" "magnification"
           , flagHelpSimple (\cmd -> cmd{help = True})
           ]
  where showHelp = "Show 16x16 bmp images"
        upd s cmdr = Right (cmdr { files = s : files cmdr })
        m s cmdr | all isDigit s = Right (cmdr { magnification = read s })
                 | otherwise     = Left "Magnification needs to be a number"

assembleMode = mode "assemble" (Assemble False [] "out.bmp")   assembleHelp (flagArg upd "[input files]")
               [ flagOpt "output" ["o","output"] o "file" "Output file"
               , flagHelpSimple (\cmd -> cmd{help = True})
               ]
  where assembleHelp = "Stack images on top of each other to make a movie " ++
                       "playable on the Game Frame"
        upd s cmdr = Right (cmdr { files  = s : files cmdr })
        o s cmdr = Right (cmdr { out = s })

argP = modes "gmfrm" (Default True) desc [showMode,assembleMode]
  where desc = "A utility program for the Game Frame"

main = do cmdr <- processArgs argP
          if help cmdr then
              print $ helpText [] HelpFormatDefault argP
            else
              case cmdr of
                Assemble { files = files 
                         , out   = out
                         } ->
                  case files of
                    [] -> putStrLn "Need at least one input file"
                    _  -> assemble files out
                Show { files         = files 
                     , magnification = m } ->
                  case files of
                    [] ->
                      do files <- takeWhileM doesFileExist
                                  [show i ++ ".bmp" | i <- [0..]]
                         if null files then
                             putStrLn "Need at least one input file"
                           else
                             showBmp files m
                    _  -> showBmp files m
                Default _ -> putStrLn "Please provide a command"

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM p [] = return []
takeWhileM p (a:as) = do b <- p a
                         if b then
                           do as' <- takeWhileM p as
                              return (a:as')
                           else
                             return []
