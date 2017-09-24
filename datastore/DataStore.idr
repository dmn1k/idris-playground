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
    addToData (x :: xs) = newItem :: x :: xs

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" args = Just (Add args) 
parseCommand "get" args = case all isDigit (unpack args) of
                               False => Nothing
                               True => Just (Get (cast args)) 
parseCommand "quit" "" = Just Quit
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = case integerToFin pos (size store) of
                          Nothing => Just ("Out of range\n", store) 
                          Just id => Just (index id (items store) ++ "\n", store) 
                    
subStrSearch : (str : String) -> (items : Vect n String) -> String 
subStrSearch str [] = "" 
subStrSearch {n = S k} str (x :: xs) = case isInfixOf str x of
                                       False => subStrSearch str xs 
                                       True => subStrSearch str xs ++ (show k) ++ ": " ++ (x ++ "\n")

search : (str : String) -> (store : DataStore) -> Maybe (String, DataStore)
search str store@(MkData size items) = Just (subStrSearch str items, store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid Command\n", store) 
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item) 
                                Just (Get pos) => getEntry pos store 
                                Just (Size) => Just (show (size store) ++ "\n", store)
                                Just (Search str) => search str store 
                                Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput 
