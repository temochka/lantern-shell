module LanternUi.Persistent exposing (Persistent(..), id, mapState, state)


type Persistent id a
    = Loading id
    | Loaded id a
    | New a


id : Persistent id a -> Maybe id
id p =
    case p of
        Loading pId ->
            Just pId

        Loaded pId _ ->
            Just pId

        New _ ->
            Nothing


state : Persistent id a -> Maybe a
state p =
    case p of
        Loading _ ->
            Nothing

        Loaded _ s ->
            Just s

        New s ->
            Just s


mapState : (a -> b) -> Persistent id a -> Persistent id b
mapState f p =
    case p of
        Loading pId ->
            Loading pId

        Loaded pId s ->
            Loaded pId (f s)

        New s ->
            New (f s)
