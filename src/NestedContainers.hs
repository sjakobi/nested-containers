module NestedContainers where

--import Data.Containers
import Data.Functor.Compose
import Data.Hashable
import Data.Key (Key(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Focus
import Prelude hiding (lookup)

class Insertable f where
  insertWith :: (a -> a -> a) -> Key f -> a -> f a -> f a

instance Ord k => Insertable (Map k) where
  insertWith = Map.insertWith

instance (Eq k, Hashable k) => Insertable (HashMap k) where
  insertWith = HashMap.insertWith

instance (Insertable f, Singletonable g, Insertable g) => Insertable (Compose f g) where
  insertWith go0 (i, j) v (Compose fg) =
    let map0 = singleton j v
        go1 _ oldMap = insertWith go0 j v oldMap
    in Compose (insertWith go1 i map0 fg)

class Singletonable f where
  singleton :: Key f -> a -> f a

instance Singletonable (Map k) where
  singleton = Map.singleton

instance Hashable k => Singletonable (HashMap k) where
  singleton = HashMap.singleton

class Focusable f where
  focus :: Focus.Strategy a r -> Key f -> f a -> (r, f a)

  insert :: Key f -> a -> f a -> f a
  insert k a f = snd (focus (Focus.insert a) k f)

  delete :: Key f -> f a -> f a
  delete k = snd . focus Focus.delete k

  lookup :: Key f -> f a -> Maybe a
  lookup k = fst . focus Focus.lookup k

  alter :: (Maybe a -> Maybe a) -> Key f -> f a -> f a
  alter f k = snd . focus (Focus.alter f) k

instance Ord k => Focusable (Map k) where
  focus s k m =
    let (r, d) = s (Map.lookup k m)
    in  case d of
          Focus.Keep -> (r, m)
          Focus.Remove -> (r, Map.delete k m)
          Focus.Replace a -> (r, Map.insert k a m)

instance (Focusable f, Focusable g, Foldable g, Singletonable g) => Focusable (Compose f g) where
  focus s (i, j) (Compose fg) =
    let minner = lookup i fg
        (r, d) = s (minner >>= lookup j)
    in  case d of
          Focus.Keep -> (r, Compose fg)
          Focus.Remove ->
            case minner of
              Nothing -> (r, Compose fg)
              Just inner ->
                case delete j inner of
                  inner'
                    | null inner' -> (r, Compose (delete i fg))
                    | otherwise -> (r, Compose (insert i inner' fg))
          Focus.Replace v' ->
            let inner =
                  case minner of
                    Nothing -> singleton j v'
                    Just inner -> insert j v' inner
            in (r, Compose (insert i inner fg))
