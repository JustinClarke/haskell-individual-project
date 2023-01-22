
-- | Module containing User Information

module UserInfo(
                    obtainUser, createNewUser,upgradeUser,obtainUserId
                  ) where

import Datatypes 
import System.Random (randomIO)

obtainUser :: [User] -> Int -> User  -- ^ Obtain a user
obtainUser users id = users !! id

createNewUser :: Int -> User -- ^ Create a new userId
createNewUser id = User id 0 []

upgradeUser :: User -> Message -> User -- ^ Upgrade the user 
upgradeUser user message
    | user_number user == to message = user {msg_recived = msg_recived user + 1, u_message = message : u_message user}
    | user_number user == from message = user {u_message = message : u_message user}
    | otherwise = user  


obtainUserId :: Int -> IO Int  -- ^ Obtain id 
obtainUserId exclude_id = do
    r <- randomIO
    let to = mod r 10 + 1 
    if to == exclude_id then
        obtainUserId exclude_id
    else
        return to

