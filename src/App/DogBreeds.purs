module App.DogBreeds
  ( Action(..)
  , UiState(..)
  , component
  , handleAction
  , initialState
  , render
  )
  where


import Prelude

import Affjax.ResponseFormat as AXRF
import Affjax.Web as AX
import Data.Argonaut (decodeJson)
import Data.Array as Array
import Data.Dog as Dog
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Pagination (Pagination)
import Data.Pagination as Pagination
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils.RemoteData (RemoteData(..))
import Utils.RemoteData as RemoteData


type State =
  { breeds :: RemoteData (Array Dog.BreedDetail) String
  , breedImages :: RemoteData Dog.BreedImageResponse String
  , uiState :: UiState
  }


data UiState
  = ListUi
  | DetailUi (Tuple Dog.Breed Int)


data Action
  = RequestBreedDetails (Tuple Dog.Breed Int)
  | ViewBreedDetails (Tuple Dog.Breed Int)
  | FetchDogBreeds
  | RetrunToListUi
  | PaginateImages Int Pagination.Paginate


component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval
        ( H.defaultEval
            { handleAction = handleAction
            , initialize = Just FetchDogBreeds
            }
        )
    }


initialState :: forall i. i -> State
initialState _ = { breeds: NotAsked, breedImages: NotAsked, uiState: ListUi }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render { breeds, uiState } =
  HH.div_ 
      [ case breeds of
          Failed error -> HH.text error
          Requested -> HH.text "Loading..."
          NotAsked -> HH.text "Error in remote data state"
          Successful breeds_ ->
            case uiState of
              DetailUi (Tuple breedName index) ->
                HH.div_ $
                  [ HH.button
                    [ HP.type_ HP.ButtonButton, HE.onClick (\_ -> RetrunToListUi) ]
                    [ HH.text "Return" ]
                  , HH.h2_ [ HH.text (show breedName) ]
                  , case Array.index breeds_ index  of
                      Nothing -> HH.text "UI state error"
                      Just (Dog.BreedDetail { images: Failed error}) -> HH.text error
                      Just  (Dog.BreedDetail { images: Requested}) -> HH.text "Loading..."
                      Just (Dog.BreedDetail { images: NotAsked }) ->HH.text "Error in remote data state"
                      Just (Dog.BreedDetail { images: Successful pagination }) ->
                        HH.div_
                          [ HH.h3_ [ HH.text ("Displaying " <> show (Pagination.getLimit pagination) <> " of " <> show (Pagination.getTotal pagination) ) ]
                          , HH.button
                            [ HP.type_ HP.ButtonButton
                            , HP.disabled (Pagination.disablePrevious pagination)
                            , HE.onClick (\_ -> (PaginateImages index Pagination.Previous))
                            ]
                            [ HH.text "Previous" ]
                          , HH.button
                            [ HP.type_ HP.ButtonButton
                            , HP.disabled (Pagination.disableNext pagination)
                            , HE.onClick (\_ -> (PaginateImages index Pagination.Next))
                            ]
                            [ HH.text "Next" ]
                          , HH.ul_ $
                            map
                              (\(Dog.BreedImageURL url) ->
                                HH.li_
                                  [ HH.img
                                    [ HP.src url ]
                                  ]
                              )
                              (Pagination.getItems pagination)
                          ]
                  ]
              ListUi ->
                HH.div_ $
                      [ HH.h2_ [ HH.text "Dog Breeds:" ]
                      , HH.ol_
                        ( Array.mapWithIndex
                          (\index (Dog.BreedDetail { name, images }) -> 
                            HH.li []
                              [ HH.button
                                [ HP.type_ HP.ButtonButton
                                , HE.onClick
                                  (\_ -> maybe (RequestBreedDetails (Tuple name index)) (\_ -> ViewBreedDetails (Tuple name index)) (maybeBreedImages images))
                                ]
                                [ HH.h3_ [ HH.text (show name) ] ]
                              ]
                          )
                          (Array.sortWith(\(Dog.BreedDetail { name }) -> show name) breeds_)
                        )
                      ]
      ]
 
maybeBreedImages :: forall e. RemoteData (Pagination Dog.BreedImageURL) e -> Maybe (Pagination Dog.BreedImageURL)
maybeBreedImages = RemoteData.toMaybe


handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  FetchDogBreeds -> do
    H.modify_ (_ { breeds = Requested })
    response <- H.liftAff $ AX.get AXRF.json "https://dog.ceo/api/breeds/list/all"
    H.modify_
      (_
        { breeds = RemoteData.fromEither $
            case response of
                Left err -> Left (AX.printError err)
                Right r -> case decodeJson r.body of
                  Left error -> 
                    Left (show error)
                  Right breeds -> Right (Dog.breedRespponseToDetails breeds)
        }
      )
  RequestBreedDetails (Tuple breedName index) -> do
    H.modify_
      (\state@{ breeds } ->
        let updatedBreeds:: RemoteData (Array Dog.BreedDetail) String
            updatedBreeds =
              RemoteData.maybeMap
                (\breeds_ ->
                  Array.modifyAt index
                      (\(Dog.BreedDetail { name }) ->
                        Dog.BreedDetail
                          { name
                          , images: Requested
                          }
                      )
                      breeds_
                ) breeds
        in
        state
          { breedImages = Requested
          , breeds = updatedBreeds
          , uiState = DetailUi (Tuple breedName index)
          }
      )
    response <- H.liftAff $ AX.get AXRF.json ("https://dog.ceo/api/breed/" <> (String.replace (String.Pattern " ") (String.Replacement "/") (show breedName)) <> "/images")
    H.modify_
      (\state@{ breeds } ->
        let
          breedImages :: RemoteData Dog.BreedImageResponse String
          breedImages =
            RemoteData.fromEither $
              case response of
                  Left err -> Left (AX.printError err)
                  Right r -> case decodeJson r.body of
                    Left error -> 
                      Left (show error)
                    Right breedImages_ -> Right breedImages_
          
          paginatedBreedImages :: RemoteData (Pagination Dog.BreedImageURL) String
          paginatedBreedImages =
            RemoteData.map (\{ message } -> Pagination.initPagination message) breedImages
          
          updatedBreeds:: RemoteData (Array Dog.BreedDetail) String
          updatedBreeds =
            RemoteData.maybeMap
              (\breeds_ ->
                Array.modifyAt index
                    (\(Dog.BreedDetail { name }) ->
                      Dog.BreedDetail
                        { name
                        , images: paginatedBreedImages
                        }
                    )
                    breeds_
              ) breeds
        in
        state
          { breedImages = breedImages
          , breeds = updatedBreeds
          }
      )
  RetrunToListUi -> H.modify_ (_ { uiState = ListUi })
  ViewBreedDetails (Tuple breedName index) ->
    H.modify_ (_ { uiState = DetailUi (Tuple breedName index) })
  PaginateImages index direction ->
    H.modify_
      (\state@{ breeds } ->
        let
          updatedBreeds:: RemoteData (Array Dog.BreedDetail) String
          updatedBreeds =
            RemoteData.maybeMap
              (\breeds_ ->
                Array.modifyAt index
                    (\(Dog.BreedDetail { name, images }) ->
                      Dog.BreedDetail
                        { name
                        , images: RemoteData.map (Pagination.paginate direction) images
                        }
                    )
                    breeds_
              ) breeds
        in
        state
          { breeds = updatedBreeds }
      )