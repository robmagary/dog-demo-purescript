module Data.Pagination
  ( Paginate(..)
  , Pagination
  , disableNext
  , disablePrevious
  , getItems
  , getLimit
  , getTotal
  , initPagination
  , paginate
  )
  where


import Prelude

import Data.Array as Array


newtype Pagination a = Pagination { limit :: Int, skip :: Int, items:: Array a }

data Paginate
    = Next
    | Previous

initPagination ::forall a. Array a -> Pagination a
initPagination items = Pagination { limit: 20, skip: 0, items: items }

paginate ::forall a. Paginate -> Pagination a -> Pagination a
paginate paginate_ (Pagination {items, limit, skip}) =
    case paginate_ of
        Next -> Pagination  {items, limit, skip: skip + limit }
        Previous -> Pagination  {items, limit, skip: skip - limit }


getItems ::forall a. Pagination a -> Array a
getItems (Pagination {items, skip, limit}) =
  Array.take limit ( Array.drop skip items)


disableNext ::forall a. Pagination a -> Boolean
disableNext (Pagination {items, skip, limit}) =
  Array.length items <= skip + limit

disablePrevious ::forall a. Pagination a -> Boolean
disablePrevious (Pagination { skip }) =
  skip <= 0

getLimit ::forall a. Pagination a -> Int
getLimit (Pagination { limit }) = limit

getTotal ::forall a. Pagination a -> Int
getTotal (Pagination { items }) = Array.length items