module HW3.Action (
  HiPermission(..),
  PermissionException(..),
  HIO(..)) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.ByteString as B
import Data.Functor ((<&>))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.IO (putStrLn)
import Data.Time.Clock (getCurrentTime)
import HW3.Base (HiAction(..), HiMonad(..), HiValue)
import HW3.Help (Dep(..))
import Prelude hiding (putStrLn)
import System.Directory
  (createDirectory, doesFileExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random (newStdGen, uniformR)

data HiPermission = AllowRead | AllowWrite | AllowTime
  deriving stock (Eq, Ord, Show)

data PermissionException = PermissionRequired HiPermission
  deriving stock (Eq, Ord, Show)
  deriving anyclass Exception

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
    via ReaderT (Set HiPermission) IO

-- | common throwIO when we don't have access to value
common :: HiPermission -> IO HiValue -> HIO HiValue
common perm result = HIO $ \set ->
  liftIO $ case Set.member perm set of
    True  -> result
    False -> throwIO (PermissionRequired perm)

instance HiMonad HIO where
  runAction (HiActionRead path) = common AllowRead $ do
    isFile <- doesFileExist path
    if isFile then do
      bytes <- B.readFile path
      return $ either (\_ -> cons bytes) cons (decodeUtf8' bytes)
    else
      listDirectory path <&> cons . S.fromList . map (cons . T.pack)

  runAction (HiActionWrite path bytes) =
    common AllowWrite (B.writeFile path bytes >> return (cons ()))

  runAction (HiActionMkDir path) =
    common AllowWrite (createDirectory path >> return (cons ()))

  runAction (HiActionChDir path) =
    common AllowRead (setCurrentDirectory path >> return (cons ()))

  runAction HiActionCwd =
    common AllowRead (getCurrentDirectory <&> cons . T.pack)

  runAction HiActionNow =
    common AllowTime (cons <$> getCurrentTime)

  runAction (HiActionRand a b) =
    newStdGen <&> cons @Rational . toEnum . fst . uniformR (a, b)

  runAction (HiActionEcho text) =
    common AllowWrite (putStrLn text >> return (cons ()))
