module Main where

import Prelude

import App.DogBreeds as DogBreeds
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

-- | Run the app.
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI DogBreeds.component unit body
