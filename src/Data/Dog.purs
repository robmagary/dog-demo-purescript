module Data.Dog where

import Prelude

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), caseJsonArray, caseJsonObject, caseJsonString, decodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Utils.Maybe as Maybe
import Data.Pagination (Pagination)
import Utils.RemoteData (RemoteData(..))

data Breed = Breed String


instance showBreed :: Show Breed where
  show (Breed breed) = breed


newtype BreedIndex =
  BreedIndex (Array Breed)


unwrapBreedIndex :: BreedIndex -> Array Breed
unwrapBreedIndex (BreedIndex breeds) = breeds


instance showBreedIndex :: Show BreedIndex where
  show (BreedIndex breeds) = show breeds


type BreedRespponse =
  { message :: BreedIndex }


data BreedImageURL = BreedImageURL String

instance showBreedImageURL :: Show BreedImageURL where
  show (BreedImageURL url) = show url

type BreedImageResponse =
    { message :: Array BreedImageURL }

newtype BreedDetail =
    BreedDetail
      { name :: Breed
      , images :: RemoteData (Pagination BreedImageURL) String
      }

breedRespponseToDetails :: BreedRespponse -> Array BreedDetail
breedRespponseToDetails { message } =
  map (\breed -> BreedDetail { name: breed, images: NotAsked }) (unwrapBreedIndex message)


-- DecodeJson


instance decodeJsonBreedIndex :: DecodeJson BreedIndex where
  decodeJson json =
     caseJsonObject (Left $ TypeMismatch "Not an object")
    (\obj -> do
      let keys = Object.keys obj
          breeds = Array.fold $ map (breedsFromKey obj) keys 
      
      pure $ BreedIndex breeds
    )
    json

breedsFromKey :: (Object.Object Json) -> String -> Array Breed
breedsFromKey obj key =
    case Object.lookup key obj of
      Nothing -> []
      Just breedJson ->
        caseJsonArray []
          (\subBreedArr ->
            case subBreedArr of
              [] -> [ Breed key ]
              _ ->
                makeSubBreeds key
                  (Maybe.unwrap $ map
                    (\subBreedJson -> 
                      caseJsonString Nothing
                        (\subBreed -> Just subBreed)
                      subBreedJson
                    ) 
                    subBreedArr
                  )
          )
          breedJson


makeSubBreeds :: String -> Array String -> Array Breed
makeSubBreeds suffix [] = [Breed suffix]
makeSubBreeds suffix prefixes = map (\prefix -> Breed (suffix <> " " <> prefix)) prefixes


instance decdoeJsonBreedImageURL :: DecodeJson BreedImageURL where
    decodeJson json = do
        url <- decodeJson json
        pure $ BreedImageURL url