module Main where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (forM_)
import Data.Set (fromList)
import HW3.Action (HIO(..), HiPermission(..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import System.Console.Haskeline
  (InputT, defaultSettings, getExternalPrint, getInputLine, outputStrLn, runInputT)

main :: IO ()
main = runInputT (defaultSettings) loop
  where
    loop = do
      minput <- getInputLine "hi> "
      forM_ minput someFunc
      loop

someFunc :: String -> InputT IO ()
someFunc (parse -> (Left err))     = outputStrLn $ show err
someFunc (parse -> (Right hiexpr)) = do
  showF <- getExternalPrint
  liftIO $ runHIO hio set >>= \case
    Left err -> showF $ show err
    Right hi -> showF $ show $ prettyValue hi
  where
    hio = eval hiexpr
    set = fromList [AllowRead, AllowWrite, AllowTime]
