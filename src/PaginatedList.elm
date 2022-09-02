module PaginatedList exposing (PaginatedList, fromList, map, params, total, values)

import Url.Builder exposing (QueryParameter)


type PaginatedList a
    = PaginatedList
        { values : List a
        , total : Int
        }


values : PaginatedList a -> List a
values (PaginatedList info) =
    info.values


total : PaginatedList a -> Int
total (PaginatedList info) =
    info.total


fromList : Int -> List a -> PaginatedList a
fromList totalCount list =
    PaginatedList { values = list, total = totalCount }


map : (a -> a) -> PaginatedList a -> PaginatedList a
map transform (PaginatedList info) =
    PaginatedList { info | values = List.map transform info.values }


params :
    { page : Int, resultsPerPage : Int }
    -> List QueryParameter
params { page, resultsPerPage } =
    let
        offset =
            (page - 1) * resultsPerPage
    in
    [ Url.Builder.string "limit" (String.fromInt resultsPerPage)
    , Url.Builder.string "offset" (String.fromInt offset)
    ]
