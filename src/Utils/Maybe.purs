module Utils.Maybe where

import Data.Array (foldl, (:))
import Data.Maybe (Maybe(..))


unwrap ::forall a. Array (Maybe a) -> Array a
unwrap arrOfMaybes =
  foldl
    (\accum m-> case m of
      Just m_ -> m_:accum
      Nothing -> accum)
    []
    arrOfMaybes
