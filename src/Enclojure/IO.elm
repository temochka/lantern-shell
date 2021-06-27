module Enclojure.IO exposing (IO(..))

import Enclojure.Located exposing (Located)
import Enclojure.Runtime as Runtime


type Syscall
    = Sleep Int


type IO
    = Const (Located Runtime.Value)



-- | IO Syscall (Runtime.Value -> Result Runtime.Exception IO)
-- | IO Syscall (Runtime.Value -> Result Runtime.Exception Runtime.Value)
-- andThen : IO ->
-- andThen callback (Pure op1) =
--     Pure
--         (\val ->
--             let
--                 result =
--                     op1 val
--             in
--             case result of
--                 Ok ret ->
--                     let
--                         (Pure op2) =
--                             callback ret
--                     in
--                     Ok op2
--                 Err e ->
--                     Err e
--         )
-- (sleep (+ 3 (fetch 5)))
-- (Pure 5 (\v -> IO (fetch v) (\v -> ))))
-- IO syscall f ->
--     IO syscall
--         (\val ->
--             let
--                 result =
--                     f val
--             in
--             case result of
--                 Ok ret ->
--                     callback ret
--                 Err e ->
--                     exception e
--         )
-- andThen : (Runtime.Env -> Runtime.Value -> IO) -> IO -> (Runtime.Env, IO)
-- andThen callback io =
--     case io of
--         Pure result ->
--             Pure result
--         Sleep _ fn ->
--             fn
-- pure 42
--     |> andThen ()
