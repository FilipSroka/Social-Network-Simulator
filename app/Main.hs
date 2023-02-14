{-|
Module      : Main
Description : This modules contains all the functionality of the program
-}


module Main (
    -- * Function
    main
    ) where

import Types
import Control.Concurrent
import Text.Printf
import System.Random
import System.Exit

main :: IO ()
main = do
    let messages = []
    allMessages <- newMVar messages
    spawnUsers 0 allMessages
    list <- takeMVar allMessages
    summarise list 1
    extraFeatures list

-- | The 'extraFeatures' function displays a menu to activate extra features
extraFeatures :: [Message] -> IO ()
extraFeatures list = do
    printf "\n"
    printf $ "=============================================\n"
    printf $ "1. See number of messages each user sent\n"
    printf $ "2. View the conversation between two users\n"
    printf $ "3. Exit\n"
    printf $ "=============================================\n"
    printf $ "Please select an option (1-3):\n"
    option <- getLine

    if option == "1" then option1 list 1
    else if option == "2" then option2 list
    else if option == "3" then exitSuccess
    else do
        printf $ "\nInvalid option!\n"
        extraFeatures list

-- | The 'option1' function displays number of messages each user sent
option1 :: [Message] -> Int -> IO ()
option1 list n = do
    if n <= 10 then do
        let count = length $ filter (\x -> sender x == show n) list 
        printf $ "User " ++ show n ++ " sent " ++ show count ++ " messages\n"
        option1 list (n+1)
    else extraFeatures list

-- | The 'option2' function displayes the conversation between two users
option2 :: [Message] -> IO ()
option2 list = do
    printf $ "Please enter the username of the first user:\n"
    user1 <- getLine
    printf $ "Please enter the username of the second user:\n"
    user2 <- getLine
    iterateList list 0 user1 user2

-- | The 'iterateList' function iterates the list to display the conversation between two users
iterateList :: [Message] -> Int -> String -> String -> IO ()
iterateList list index user1 user2 = do
    if index == length list then extraFeatures list
    else if sender (list !! index) == user1 && receiver (list !! index) == user2 then
        printf $ "[" ++ user1 ++ "]: " ++ text (list !! index) ++ "\n"
    else if sender (list !! index) == user2 && receiver (list !! index) == user1 then
        printf $ "[" ++ user2 ++ "]: " ++ text (list !! index) ++ "\n"
    else iterateList list (index+1) user1 user2 
    iterateList list (index+1) user1 user2 

-- | The 'spawnUsers' function spawns 10 users
spawnUsers :: Int -> MVar [Message] -> IO ()
spawnUsers n allMessages = do
    if n == 10 then return ()
    else do 
        flag <- newEmptyMVar
        _ <- forkIO $ threadsBehaviour (n+1) flag allMessages
        spawnUsers (n+1) allMessages
        takeMVar flag

-- | The 'summarise' function displays number of messages each user received
summarise :: [Message] -> Int -> IO ()
summarise list n = do
    if n <= 10 then do
        let count = length $ filter (\x -> receiver x == show n) list 
        printf $ "User " ++ show n ++ " received " ++ show count ++ " messages\n"
        summarise list (n+1)
    else return ()

-- | The 'threadsBehaviour' function controls the way the thread behaves 
threadsBehaviour :: Int -> MVar () -> MVar [Message] -> IO ()
threadsBehaviour n flag allMessages = do
    let user = User{username=show n}
    -- printf $ "created User " ++ show user ++ "\n"
    threadsOperations user allMessages
    putMVar flag ()
    return ()

-- | The 'threadOperations' function controls the way the thread operates
threadsOperations :: User -> MVar [Message] -> IO ()
threadsOperations user allMessages = do
    setWait
    target <- pickUser $ read (username user)
    sendMessage user (show target) allMessages 

-- | The 'threadOperations' function sends a messages to another user
sendMessage :: User -> String -> MVar [Message] -> IO ()
sendMessage origin target allMessages = do
    message <- generateMessage
    list <- takeMVar allMessages
    if length list < 100 then do
        -- printf $ username sender ++ " " ++ message ++ " " ++ receiver ++ "\n"
        let newList = list ++ [Message{text=message, sender=username origin, receiver=target}]
        putMVar allMessages newList
        threadsOperations origin allMessages
    else
        putMVar allMessages list

-- | The 'threadOperations' function generates a messages to be sent
generateMessage :: IO String
generateMessage = do
    let messages = ['A'..'Z']
    t <- randomIO :: IO Float
    i <- toInt $ fromIntegral ((length messages) - 1) * t
    let message = (messages !! i) : ""
    return message

-- | The 'threadOperations' function randomly picks a user to send a message to
pickUser :: Int -> IO Int
pickUser n = do
    t <- randomIO :: IO Float
    i <- toInt $ 9 * t + 1
    if i == n then pickUser n else return i

-- | The 'threadOperations' fucntion makes the thread wait for a random amount of time
setWait :: IO ()
setWait = do
    t <- randomIO :: IO Float
    -- printf $ "waiting " ++ show t ++ " seconds\n"
    i <- toInt $ t * 1000000
    threadDelay i                 

-- | The 'threadOperations' function converts Float to Int
toInt :: Float -> IO Int
toInt n = do
    return (round n)
