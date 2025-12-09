module Web.Kamyu.Combinators
    (get, post, put, delete, patch
    , path, capture
    , root
    , middleware
    ) where

import Web.Kamyu.Core
    ( Kamyu,
      Middleware,
      KamyuHandler,
      Method(PATCH, GET, POST, PUT, DELETE),
      addRoute )

get :: String -> KamyuHandler -> Kamyu()
get = addRoute GET

post :: String -> KamyuHandler -> Kamyu()
post = addRoute POST

put :: String -> KamyuHandler -> Kamyu ()
put = addRoute PUT

delete :: String -> KamyuHandler -> Kamyu ()
delete = addRoute DELETE

patch :: String -> KamyuHandler -> Kamyu ()
patch = addRoute PATCH

path :: String -> Kamyu a -> Kamyu a
path = undefined

capture :: String -> Kamyu a -> Kamyu a
capture = undefined

--TODO: some combinators for edsl
root :: Kamyu a -> Kamyu a
root = undefined

middleware :: Middleware -> Kamyu ()
middleware = undefined