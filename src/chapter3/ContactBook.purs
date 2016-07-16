module Chapter3.ContactBook where

import Prelude

import Control.Plus (empty)
import Data.List (List, filter, head, (:), null, nubBy)
import Data.Maybe (Maybe)

type Entry = 
    { firstName :: String,
      lastName  :: String,
      address   :: Address
    }

type Address = 
    { street    :: String,
      city      :: String,
      state     :: String
    }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.firstName   <> ", " <> 
                  entry.lastName    <> ": " <> 
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

emptyBook :: AddressBook
emptyBook = empty

--the transition from "insertentry a b" to "(:)" is an eta conversion
insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = (:)

findEntry :: (Entry -> Boolean) -> AddressBook -> Maybe Entry
findEntry isMatch = head <<< filter isMatch 

findByName :: String -> String -> AddressBook -> Maybe Entry
findByName first last = findEntry sameName
    where 
    sameName e = e.firstName == first && e.lastName == last

findByAddress :: Address -> AddressBook -> Maybe Entry
findByAddress addr = findEntry sameAddress
    where 
    sameAddress e = e.address.street == addr.street &&
                    e.address.city   == addr.city   &&
                    e.address.state  == addr.street

nameExists :: String -> AddressBook -> Boolean
nameExists name = null <<< filter partialNameMatch
    where
    partialNameMatch e = e.firstName == name ||
                         e.lastName  == name

removeDupes :: AddressBook -> AddressBook
removeDupes = nubBy sameName
    where 
    sameName e e' = e.firstName == e'.firstName &&
                    e.lastName  == e'.lastName
