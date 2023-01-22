

-- | Module containing output generation process

module Output(randomNum,mapForkMethod,upgradeInfo,yieldProcess 
                  ) where

import Datatypes 
import System.Random (randomIO,randomRIO, random)
import Control.Concurrent
import Control.Monad
import UserInfo
import Text.StringRandom
import Data.Text (pack)

randomNum :: Int -> IO Int  -- ^ Generate a random number  
randomNum maxVal = do 
    r <- randomIO 
    let to = mod r 100 + 1 
    return to

mapForkMethod :: MVar [User] -> MVar Int -> MVar Int -> [Int] -> IO () -- ^ Map for each number from 0 to 10
mapForkMethod users outcome total user_threads =
    forM_ user_threads $ \n ->    
    forkIO (yieldProcess n users outcome total)

upgradeInfo :: MVar q -> q -> IO () -- ^ Upgrade the information
upgradeInfo = putMVar

yieldProcess :: Int -> MVar [User] -> MVar Int -> MVar Int -> IO () -- ^ Send and receive the messages
yieldProcess user_number users outcome total = do
    sum <- takeMVar total
    enduser <- takeMVar users
    if sum == 100 then do
        putStrLn  "Process Finished" 
        putStrLn  "<<--------------------------------->>" 

        putMVar outcome user_number
        upgradeInfo users enduser 
        return ()
    else do
        
        
        to <- obtainUserId user_number
        let regex = pack "[a-z]{10,20}$"
        randStr <- stringRandomIO regex
        let message = Message {to=to, from=user_number, content=randStr}
        putStrLn $ "\n Message Coming From: " ++ show user_number ++ "\n Message To: " ++ show to ++ "\n Message Content: " ++ show randStr

        let enduser' = map helper enduser
            helper c = upgradeUser c message
        let sum' = sum + 1
        putMVar total sum'
        putMVar users enduser'
        delayTime <- randomRIO (7000, 85000)  
        putStrLn $ "Random time taken is ==> " ++ show delayTime ++ " Millisecond"
        threadDelay delayTime 
        yieldProcess user_number users outcome total
        

