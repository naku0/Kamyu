module Web.Kamyu.Combinators
  ( get,
    post,
    put,
    delete,
    patch,
    path,
    capture,
    root,
    createMiddleware,
  )
where

import Web.Kamyu.Core
  ( Kamyu,
    KamyuHandler,
    Method (DELETE, GET, PATCH, POST, PUT),
    Middleware,
    addMiddleware,
    addRoute,
    withPathContext,
    withRootContext,
  )

get :: String -> KamyuHandler -> Kamyu ()
get = addRoute GET

post :: String -> KamyuHandler -> Kamyu ()
post = addRoute POST

put :: String -> KamyuHandler -> Kamyu ()
put = addRoute PUT

delete :: String -> KamyuHandler -> Kamyu ()
delete = addRoute DELETE

patch :: String -> KamyuHandler -> Kamyu ()
patch = addRoute PATCH

path :: String -> Kamyu a -> Kamyu a
path = withPathContext

capture :: String -> Kamyu a -> Kamyu a
capture name = withPathContext (':' : name)

root :: Kamyu a -> Kamyu a
root = withRootContext

createMiddleware :: Middleware -> Kamyu ()
createMiddleware = addMiddleware
