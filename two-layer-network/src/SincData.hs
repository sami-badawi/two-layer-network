module SincData where

import Control.Lens (element, view, (^?))
import Control.Monad.Trans.State (StateT (..), evalStateT, gets, state)
import Control.Monad (ap, replicateM, void)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Random.Normal (normal)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import System.Random (Random, RandomGen, getStdGen, random)
import Torch.Data.Pipeline (Dataset (..), MapStyleOptions (..), Sample (..), makeListT, mapStyleOpts)

-- | Compute the sine cardinal (sinc) function,
-- see https://mathworld.wolfram.com/SincFunction.html
sinc :: Floating a => a -> a
sinc a = sin a / a

-- | Compute the sine cardinal (sinc) function and add normally distributed noise
-- of strength epsilon
noisySinc :: (Floating a, Random a, RandomGen g) => a -> a -> g -> (a, g)
noisySinc eps a g = let (noise, g') = normal g in (sinc a + eps * noise, g')

-- | Datatype to represent a dataset of sine cardinal (sinc) inputs and outputs
data SincData = SincData {name :: Text, unSincData :: [(Float, Float)]} deriving (Eq, Ord)

-- | Create a dataset of noisy sine cardinal (sinc) values of a desired size
mkSincData :: (RandomGen g, Monad m) => Text -> Int -> StateT g m SincData
mkSincData name size =
  let next = do
        x <- (* 20) <$> state normal
        y <- state (noisySinc 0.05 x)
        pure (x, y)
   in SincData name <$> replicateM size next

-- | 'Dataset' instance used for streaming sine cardinal (sinc) examples
instance MonadFail m => Dataset m SincData Int (Float, Float) where
  getItem (SincData _ d) k = maybe (fail "invalid key") pure $ d ^? element k
  keys (SincData _ d) = Set.fromList [0 .. Prelude.length d -1]

