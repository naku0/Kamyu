# 🌊 Kamyu

**Be simple, write less**

Kamyu — это легкий и интуитивно понятный веб-фреймворк, который позволяет создавать веб-приложения на Haskell без необходимости глубокого погружения в сложные языковые концепции.

Основная идея: написать технологию, которой можно спокойно пользоваться не зная Haskell.

## 🚀 Быстрый старт

```haskell

module Main where

import Kamyu

homeHandler :: KamyuHandler
homeHandler _ = do
     putStrLn "Calling home"
     return $ ok "Home is here"

main :: IO ()
main = do
    putStrLn "=== KAMYU START ==="
    runKamyu 8080 $ do

        get "/" $ \_ -> do
            putStrLn "⭐ Handler for GET / called!"
            return $ ok "SUCCESS! Kamyu is working!"

        get "/home" homeHandler

```

## 🎯 Основные возможности

1. Маршрутизация

```haskell
get "/users" usersHandler
post "/users" createUserHandler
put "/users/:id" updateUserHandler
delete "/users/:id" deleteUserHandler
```

2. Простые обработчики

```haskell
helloHandler :: KamyuHandler
helloHandler _ = do
    return $ responseLBS status200 [] "Hello World!"
```

3. JSON-обработчики (по мотивам Spring)

```haskell

import Web.Kamyu.Json (jsonWithStatus)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, status201)

-- Что ожидаем получить в теле POST-запроса
data CreatePerson = CreatePerson { name :: String, age :: Int }
    deriving (Generic)
instance FromJSON CreatePerson

-- Что вернём клиенту
data Person = Person { identifier :: Int, fullName :: String, personAge :: Int }
    deriving (Generic)
instance ToJSON Person

-- Обработчик: принимает CreatePerson, возвращает IO (Status, Person)
createPersonHandler :: CreatePerson -> IO (Status, Person)
createPersonHandler body = do
    let personName = name body
        personAge = age body
    return (status201, Person 1 personName personAge)

-- jsonWithStatus createPersonHandler :: KamyuHandler
post "/people" $ jsonWithStatus createPersonHandler
```

`jsonWithStatus` сам вызывает `createPersonHandler`, 
предварительно десериализовав тело запроса в `CreatePerson`. 
Обработчик возвращает пару `(HTTP Status, Person)`, 
что позволяет выбрать нужный код ответа (например, 201). 
Kamyu автоматически сериализует результат в JSON 
и выставляет `Content-Type: application/json`. 
Для случая по умолчанию (HTTP 200) используйте `json`

## 🛣️ Roadmap

✅ Базовая маршрутизация (GET, POST, PUT, DELETE)

✅ Простые HTTP-ответы

🚧 Удобные хелперы для статусов (ok, created, notFound)

🚧 Парсинг параметров запроса

🚧 Middleware поддержка

✅ JSON (де)сериализация

🚧 Статические файлы


> [!WARNING]
> Это тестовая версия и она будет улучшаться
