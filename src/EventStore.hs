{-# LANGUAGE ScopedTypeVariables #-}

module EventStore where

import Control.Concurrent.STM
import Control.Monad (forever, void)
import Eventful
import Eventful.Store.Memory

import Data.Card
import Data.Pack

main :: IO ()
main
  -- Create the event store and run loop forever
 = do
  tvar <- eventMapTVar
  let writer = tvarEventStoreWriter tvar
      reader = tvarEventStoreReader tvar
  forever (readAndHandleCommand writer reader)

events =
  [ Initialized
      [ (Card "Axe")
      , (Card "Cunning Plan")
      , (Card "Cunning Plan")
      , (Card "Selfish Cleric")
      ]
  , Picked (Card "Axe") (Card "Cunning Plan")
  , Transformed (Card "Cunning Plan") (Card "Drow Ranger")
  ]

readAndHandleCommand ::
     EventStoreWriter STM PackEvent
  -> VersionedEventStoreReader STM PackEvent
  -> IO ()
readAndHandleCommand writer reader
  -- Just use the nil uuid for everything
 = do
  let uuid = nil
  -- Get current state and print it out
  latestStreamProjection <-
    atomically $
    getLatestStreamProjection
      reader
      (versionedStreamProjection uuid packProjection)
  let currentState = streamProjectionState latestStreamProjection
  putStrLn $ "Current state: " ++ show currentState
  -- Ask user for command
  putStrLn
    "Enter a command. (IncrementCounter n, DecrementCounter n, ResetCounter):"
  input <- getLine
  -- Parse command and handle 
  putStrLn $ "Events generated: " ++ show events
  void . atomically $ storeEvents writer AnyVersion uuid events
  putStrLn ""
