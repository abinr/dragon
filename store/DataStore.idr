module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

data Command
  = Add String
  | Get Integer
  | Size
  | Search String
  | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" args = Just (Add args)
parseCommand "get" args = case all isDigit (unpack args) of
                               False => Nothing
                               True => Just (Get (cast args))
parseCommand "size" _ = Just Size
parseCommand "search" args = Just (Search args)
parseCommand "quit" _ = Just Quit
parseCommand _ _ = Nothing

parse : (input : String ) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (trim args)

getEntry : (pos : Integer) -> (store : DataStore) -> (input : String) -> Maybe (String, DataStore)
getEntry pos store input =
  let
    storeItems = items store
  in
    case integerToFin pos (size store) of
          Nothing => Just ("Out of Range", store)
          (Just id) => Just (index id storeItems ++ "\n", store)

searchEntries : (txt : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchEntries txt store = 
  let
    indices = findIndices (Strings.isInfixOf txt) (items store)
    matches = 
      map (\i => show (finToInteger i) ++ ": " ++ (index i (items store)) ++ " ") indices
    results = concat matches ++ "\n"
  in
    Just (results, store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = 
  case parse input of
    Nothing => Just ("Invalid Command:\n", store)
    (Just (Add item)) => 
      Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    (Just (Search txt)) => searchEntries txt store
    (Just (Get pos)) => getEntry pos store input
    (Just Size) => Just ("Size: " ++ show (size store) ++ "\n", store)
    (Just Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
  
