module Utils.RemoteData
  ( RemoteData(..)
  , fromEither
  , map
  , maybeMap
  , toMaybe
  )
  where


import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)

data RemoteData a e
    = NotAsked
    | Requested
    | Successful a
    | Failed e


fromEither :: forall a e. Either e a -> RemoteData a e
fromEither = either Failed Successful

map :: forall a b e. (a -> b) -> RemoteData a e -> RemoteData b e
map f remoteA =
    case remoteA of
        Successful a ->
            Successful (f a)
        
        NotAsked ->
            NotAsked
        
        Requested ->
            Requested
        
        Failed e ->
            Failed e


maybeMap :: forall a e. (a -> Maybe a) -> RemoteData a e -> RemoteData a e
maybeMap f remoteA =
    case remoteA of
        Successful a ->
            case f a of
                Just a ->
                    Successful a
                
                Nothing ->
                    remoteA
        
        NotAsked ->
            NotAsked
        
        Requested ->
            Requested
        
        Failed e ->
            Failed e

toMaybe :: forall a e. RemoteData a e -> Maybe a
toMaybe remoteA =
    case remoteA of
        Successful a ->
            Just a
        
        _ ->
            Nothing