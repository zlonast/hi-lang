module HW3.Action where

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
import HW3.Base (HiAction(..), HiMonad(..))
import HW3.Help (Dep(..))
import Prelude hiding (putStrLn)
import System.Directory
  (createDirectory, doesFileExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random (newStdGen, uniformR)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Ord, Show)

data PermissionException = PermissionRequired HiPermission
  deriving (Eq, Ord, Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving stock Functor
  deriving Applicative via ReaderT (Set HiPermission) IO
  deriving Monad       via ReaderT (Set HiPermission) IO
  deriving MonadIO     via ReaderT (Set HiPermission) IO

ask :: HIO (Set HiPermission)
ask = HIO $ \set -> return set

instance HiMonad HIO where
  runAction (HiActionRead path) = do
    set <- ask
    liftIO $ if Set.member AllowRead set then do
      isFile <- doesFileExist path
      if isFile then do
        bytes <- B.readFile path
        return $ either (\_ -> cons bytes) cons (decodeUtf8' bytes)
      else
        listDirectory path <&> cons . S.fromList . map (cons . T.pack)
    else
      throwIO (PermissionRequired AllowRead)

  runAction (HiActionWrite path bytes) = do
    set <- ask
    liftIO $ if Set.member AllowWrite set then
      B.writeFile path bytes >> return (cons ())
    else
      throwIO (PermissionRequired AllowWrite)

  runAction (HiActionMkDir path) = do
    set <- ask
    liftIO $ if Set.member AllowWrite set then
      createDirectory path >> return (cons ())
    else
      throwIO (PermissionRequired AllowWrite)

  runAction (HiActionChDir path) = do
    set <- ask
    liftIO $ if Set.member AllowRead set then
      setCurrentDirectory path >> return (cons ())
    else
      throwIO (PermissionRequired AllowRead)

  runAction HiActionCwd = do
    set <- ask
    liftIO $ if Set.member AllowRead set then
      getCurrentDirectory <&> cons . T.pack
    else
      throwIO (PermissionRequired AllowRead)

  runAction HiActionNow = do
    set <- ask
    liftIO $ if Set.member AllowTime set then
      cons <$> getCurrentTime
    else
      throwIO (PermissionRequired AllowTime)

  runAction (HiActionRand a b) = do
    newStdGen <&> cons @Rational . toEnum . fst . uniformR (a, b)

  runAction (HiActionEcho text) = do
    set <- ask
    liftIO $ if Set.member AllowWrite set then
      putStrLn text >> return (cons ())
    else
      throwIO (PermissionRequired AllowWrite)
