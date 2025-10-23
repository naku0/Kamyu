# 🌊 Kamyu

**Be simple, write less**

Kamyu — это легкий и интуитивно понятный веб-фреймворк, который позволяет создавать веб-приложения на Haskell без необходимости глубокого погружения в сложные языковые концепции.

Основная идея: написать технологию, которой можно спокойно пользоваться не зная Haskell.

## 🚀 Быстрый старт

```haskell
{-# LANGUAGE OverloadedStrings #-}

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

## 🛣️ Roadmap

✅ Базовая маршрутизация (GET, POST, PUT, DELETE)

✅ Простые HTTP-ответы

🚧 Удобные хелперы для статусов (ok, created, notFound)

🚧 Парсинг параметров запроса

🚧 Middleware поддержка

🚧 JSON сериализация

🚧 Статические файлы


> [!WARNING]
> Это тестовая версия и она будет улучшаться
