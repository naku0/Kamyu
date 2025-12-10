# 🌊 Kamyu

**Be simple, write less**

Kamyu — это легкий и интуитивно понятный веб-фреймворк, который позволяет создавать веб-приложения на Haskell без необходимости глубокого погружения в сложные языковые концепции.

Основная идея: написать технологию, которой можно спокойно пользоваться не зная Haskell.

## 🚀 Быстрый старт

```haskell

module Main where

import Kamyu

main :: IO ()
main = do
    putStrLn "=== KAMYU START ==="
    runKamyu 8080 $ do

        get "/" $ \_ -> do
            putStrLn "⭐ Handler for GET / called!"
            return $ ok "SUCCESS! Kamyu is working!"

```

## 📖 Документация

1. Маршрутизация

```haskell
get "/" homeHandler
post "/users" createUserHandler
get "/users/:id" getUserHandler
```

2. Параметры пути

```haskell
get "/user/:id" $ \_ params -> do
    let userId = pathParamDef "0" "id" params
    return $ ok $ "User ID: " ++ userId
```

3. Query параметры

```haskell
get "/search" $ \req params -> do
    let query = getString "q" req `orElse` ""
        page = getInt "page" req `orElse` 1
    return $ ok $ "Search: " ++ query ++ ", page: " ++ show page
```

4. JSON-обработчики

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Web.Kamyu.Json (jsonHandler, JsonCodec)
import Web.Kamyu.Params (orElse, getString, pathParamDef)
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status201)
import Network.Wai (Request)

-- Что ожидаем получить в теле POST-запроса
data CreatePerson = CreatePerson { name :: String, age :: Int }
    deriving (Generic, JsonCodec)

-- Что вернём клиенту
data Person = Person { identifier :: Int, fullName :: String, personAge :: Int }
    deriving (Generic, JsonCodec)

-- Универсальный обработчик: JSON + query + path params
createPersonHandler :: CreatePerson -> Request -> [(String, String)] -> IO (Status, Person)
createPersonHandler body req pathParams = do
    let personName = name body
        personAge = age body
        sourceTag = orElse (getString "source" req) "api"
        citySlug = pathParamDef "unknown" "city" pathParams
    putStrLn $ "Source tag: " ++ sourceTag ++ ", city: " ++ citySlug
    return (status201, Person 1 (personName ++ " from " ++ citySlug) personAge)

-- jsonHandler createPersonHandler :: KamyuHandler
post "/cities/:city/people" $ jsonHandler createPersonHandler
```

`jsonHandler` сам десериализует тело запроса, передаёт распарсенный `CreatePerson`,
исходный `Request` и path params. Обработчик возвращает пару `(HTTP Status, Person)` —
можно игнорировать дополнительные аргументы или использовать их для чтения query
параметров, заголовков и динамических сегментов пути. Kamyu автоматически
сериализует результат и выставляет `Content-Type: application/json`.

## 🛣️ Roadmap

✅ Базовая маршрутизация (GET, POST, PUT, DELETE)

✅ Простые HTTP-ответы

⏳ Удобные хелперы для статусов (ok, created, notFound)

✅ Парсинг параметров запроса

🚧 Middleware поддержка

✅ JSON (де)сериализация

🚧 Статические файлы


> [!WARNING]
> Это тестовая версия и она будет улучшаться
