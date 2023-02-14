{-|
Module      : Types
Description : This module includes all the type definitions required throughout the program
-}

module Types ( 
    -- * Types
    User (..),
    Message (..),
) where

-- | This is the data structure that represents a User
data User = User {
    -- | The 'username' method returns a username by which user is uniquely identified
    username :: String
} deriving (Show)

-- | This is the data structure that represents a Message
data Message = Message {
    -- | The 'text' method returns a text contained within the message
    text :: String,
    -- | The 'sender' method returns a username associated with the sender of the message
    sender :: String,
    -- | The 'receiver' method returns a username associated with the receiver of the message
    receiver :: String
} deriving (Show)

