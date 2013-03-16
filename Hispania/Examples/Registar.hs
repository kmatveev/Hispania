module Hispania.Examples.Registar where

import Hispania.Stack
import Hispania.Types
import Hispania.Transport
import Hispania.Parser

import Hispania.TestTypes

import Data.ByteString.Char8 as BS
import Network.BSD
import Network.Socket

import Data.Maybe
import qualified Data.Map as Map



registarHandler :: Map.Map Header Header -> RequestHandler
registarHandler storage req serverCtx stack = 
        case fromHeader of
          Nothing   -> respond serverCtx (respondTo req 400) stack
          Just user -> let updatedStack = maybe stack (\x -> stack{requestHandler = (registarHandler (Map.insert user x storage))}) receivedAddr
                           cleanResponse = respondTo req 200
                           populatedResponse = maybe cleanResponse (\x -> appendHeader cleanResponse x) (getBinding user)
                       in respond serverCtx populatedResponse updatedStack
  where
    fromHeader = getTopHeader req (BS.pack "From")
    receivedAddr = getTopHeader req (BS.pack "Contact")
    getBinding key = Map.lookup key storage >>= (\x -> Just ((BS.pack "Contact"), x))


initStack :: Stack -> IO Stack
initStack stack = do
              sock <- socket AF_INET Datagram 0
              bindSocket sock localAddr
              return (updateTransport (addSocketUDP localAddr sock) stack)
   where
     localAddr = SockAddrInet 5657 iNADDR_ANY

start :: Stack -> IO ()
start stack  = do
                 initilaizedStack <- initStack stack
                 serveLoop initilaizedStack

testRegistar = start (newStack (registarHandler Map.empty))

