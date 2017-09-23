module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect oldLen String -> Vect (S oldLen) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs 

data Command = Add String
             | Get Integer
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" args = Just (Add args) 
parseCommand "get" args = case all isDigit (unpack args) of
                               False => Nothing
                               True => Just (Get (cast args)) 
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = case integerToFin pos (size store) of
                          Nothing => Just ("Out of range\n", store) 
                          Just id => Just (index id (items store) ++ "\n", store) 

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid Command\n", store) 
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item) 
                                Just (Get pos) => getEntry pos store 
                                Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput 
