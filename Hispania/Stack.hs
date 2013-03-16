module Hispania.Stack where

import Hispania.Types
import Hispania.Parser
import Hispania.Transport
import Hispania.Util

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSW
import qualified Data.Map as Map
import Blaze.ByteString.Builder
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD
import qualified Data.Attoparsec.ByteString as ATP
import qualified Data.Attoparsec.ByteString.Char8 as ATPC


import Control.Monad.State
import Data.Word

import Data.Maybe

echoPort = 9900

data Stack = Stack {
       transportLayer::TransportLayer, 
       requestHandler::RequestHandler, 
       generatorVal::Word, 
       clientTranMap :: ClientTranMap, 
       serverTranMap :: ServerTranMap, 
       dialogMap :: DialogMap,
       proxyMap :: ProxyMap
}

updateTransport :: (TransportLayer -> TransportLayer) -> Stack -> Stack
updateTransport f stack = (liftM7 Stack (f . transportLayer) requestHandler generatorVal clientTranMap serverTranMap dialogMap proxyMap) stack


updateGenerator :: Word -> Stack -> Stack
updateGenerator gen stack = (liftM7 Stack transportLayer requestHandler (return gen) clientTranMap serverTranMap dialogMap proxyMap) stack

updateClientTran :: (ClientTranKey, ClientTransaction) -> Stack -> Stack
updateClientTran (key, val) stack = (liftM7 Stack transportLayer requestHandler generatorVal ((Map.insert key val) . clientTranMap) serverTranMap dialogMap proxyMap) stack

updateClientTranMap :: ClientTranMap -> Stack -> Stack
updateClientTranMap updatedTranMap stack = stack{clientTranMap=updatedTranMap}


newStack :: RequestHandler -> Stack
newStack reqHandler = Stack clearTransportLayer reqHandler 0 Map.empty Map.empty Map.empty Map.empty

type ClientTranMap = Map.Map ClientTranKey ClientTransaction

type ServerTranMap = Map.Map BS.ByteString ServerTransaction

type DialogMap = Map.Map DialogKey Dialog

type ProxyMap = Map.Map ProxyKey Proxy

getClientContext :: ClientTransaction -> ClientContext
getClientContext (InviteClientTransaction state clientCtx) = clientCtx
getClientContext (NonInviteClientTransaction state clientCtx) = clientCtx

data Dialog = Dialog

data Proxy = Proxy

data ServerContext = ServerContext Stack Direction Transport RequestHandler

class Context a where
  extractStack :: a -> Stack

instance Context ServerContext where
  extractStack (ServerContext stack _ _ _) = stack

getRequestHandler (ServerContext _ _ _ reqHandler) = reqHandler


serveLoop :: Stack -> IO ()
serveLoop origStack = do
           updatedStack <- serveOne origStack
           serveLoop updatedStack

serveOne :: Stack -> IO Stack
serveOne origStack = do
           (mesg, direction, transport) <- receive (transportLayer origStack)
           putStrLn ("Received message " ++ (show direction))
           handle mesg origStack direction transport



handle :: BS.ByteString -> Stack -> Direction -> Transport -> IO Stack
handle bytes stack direction transport = case parseResult of
                                           ATPC.Done _ (Right request) -> handleRequest request (ServerContext stack direction transport (requestHandler stack)) stack
                                           ATPC.Done _ (Left response) -> handleResponse response stack
                                           ATPC.Fail _ _ errorStr -> error  ("Failed to parse:" ++ errorStr)
                                           ATPC.Partial _ -> error "Parsed only partially"
                       where
                         parseResult = ATPC.parse messageParser bytes


handleRequest :: Request () -> ServerContext -> Stack -> IO Stack
handleRequest request serverCtx stack = (getRequestHandler serverCtx) request serverCtx stack


handleResponse :: Response () -> Stack -> IO Stack
handleResponse res stack = case clientTranResult of
                             Nothing -> return stack
                             Just clientTran -> do
                                                 return (responseHandler (getClientContext clientTran))
                                                 return stack
  where
    clientTranKey = fromJust (getClientTranKey res)
    clientTranResult = Map.lookup clientTranKey (clientTranMap stack)
    

type RequestHandler = Request () -> ServerContext -> Stack -> IO Stack

respond :: ServerContext -> Response () -> Stack -> IO Stack
respond serverCtx response stack = do
       updatedTransportLayer <- sendResponseTo response respDirection transport (transportLayer stack)
       return (updateTransport (\_ -> updatedTransportLayer) stack)
   where
      (ServerContext stack direction transport _) = serverCtx
      respDirection = calcDirectionForResponse transport direction


calcDirectionForResponse :: Transport -> Direction -> Direction
calcDirectionForResponse TCP reqDirection = reverseDirection reqDirection
calcDirectionForResponse UDP reqDirection = Direction (fromJust (source reqDirection), Nothing)

sendRequestTo :: Request () -> Direction -> Transport -> TransportLayer -> IO TransportLayer
sendRequestTo =  sendRawTo . toByteString . requestBuilder2

sendResponseTo :: Response () -> Direction -> Transport -> TransportLayer -> IO TransportLayer 
sendResponseTo = sendRawTo . toByteString . responseBuilder2


respondTo :: Request () -> Int -> Response ()
respondTo req status = Response defaultProtoVersion status (defaultReason status) (copyFromRequest (reqHeaders req)) ()
  where 
     shouldCopy name = (name == "Via") || (name == "From") || (name == "CSeq") || (name == "To") || (name == "Call-ID")
     copyFromRequest = filter (shouldCopy . BS.unpack . fst)
   

getTargetURI :: Request a -> URI
getTargetURI req = case (getTopHeader req (BS.pack "Route")) of
                     Just (GenericHeader value) -> case (ATPC.parseOnly uriParser value) of
                                                          Left _ -> reqURI req
                                                          Right parsedUri -> parsedUri
                     Just (AddressHeader value) -> addrURI value
                     Nothing -> reqURI req                                           

resolveURI :: URI -> IO HostAddress
resolveURI (RawURI schema specific) = do
    entry <- getHostByName (BS.unpack specific) 
    return (head . hostAddresses $ entry)
resolveURI (SipURI secure user pass host port params) = do
       entry <- getHostByName (BS.unpack host) 
       return (head . hostAddresses $ entry)


getUriPort :: URI -> PortNumber
getUriPort uri = 5060

chooseTransport :: URI -> Transport
chooseTransport _ = UDP

getDirection :: Request a -> IO (Direction, Transport)
getDirection req = do
          hostAddr <- resolveURI targetURI
          return (Direction ((SockAddrInet port hostAddr), Nothing), transport)
    where 
      targetURI = getTargetURI req
      port      = getUriPort targetURI
      transport = chooseTransport targetURI

sendRequest :: ResponseHandler -> Request () -> Stack -> IO Stack
sendRequest handler req stack = do
              (direction, transport) <- getDirection req
              updatedTransportLayer <- sendRequestTo req direction transport (transportLayer stack)
              return (updateTransport (\_ -> updatedTransportLayer) stack)


createRequest :: RequestMethod -> URI -> URI -> Stack -> (Request (), Stack)
createRequest method from to stack = (Request method to defaultProtoVersion headers (), updateGenerator newGenVal stack)
  where
     ((callId, branch, localTag), newGenVal) = runState ( let stateGen = state nextVal in liftM3 (,,) stateGen stateGen stateGen )  (generatorVal stack)
     callIdHdr = ((BS.pack "Call-ID"), (GenericHeader (packThem callId)))
     fromHdr   = ((BS.pack "From") , (GenericHeader (packThem localTag)))
     cseqHdr   = ((BS.pack "CSeq"), (GenericHeader (BS.pack "1")))
     headers   = callIdHdr : fromHdr : cseqHdr : []


createUAContext:: SipAddress -> Stack -> (UAContext, Stack)
createUAContext local stack = ( (UAContext updatedLocal (packThem callIdTmp)), (updateGenerator newGenVal stack) )
  where
     ((callIdTmp, localTagTmp), newGenVal) = runState ( let stateGen = state nextVal in liftM2 (,) stateGen stateGen )  (generatorVal stack)    
     updatedLocal = addAddressParam (BS.pack "tag") (packThem localTagTmp) local

createClientContext:: SipAddress -> RequestMethod -> ResponseHandler -> (UAContext, Stack) -> (ClientContext, Stack)
createClientContext remote method respHandler (ua, stack) = ((ClientContext remote ua respHandler (Request method requestURI defaultProtoVersion headers ())), stack)
   where
      callIdHdr  = ((BS.pack "Call-ID") , GenericHeader (callIdVal ua))
      fromHdr    = ((BS.pack "From")    , AddressHeader (localAddr ua))
      cseqHdr    = ((BS.pack "CSeq")    , GenericHeader  (BS.pack "1"))
      toHdr      = ((BS.pack "To")      , AddressHeader remote)
      headers    = callIdHdr : fromHdr : cseqHdr : toHdr : []
      requestURI = addrURI remote

initClientTran :: Request a -> ClientContext -> ClientTransaction
initClientTran req ctx = 
   case (reqMethod req) of
     INVITE -> InviteClientTransaction ICInitial ctx
     _      -> NonInviteClientTransaction NICInitial ctx

sendReq :: Request () -> (ClientContext, Stack) -> IO Stack
sendReq req (clientCtx, stack) = 
            do
              (direction, transport) <- getDirection req
              (sock, updatedTransportLayer) <- getSocket transport direction (transportLayer stack)
              localAddr <- getSocketName sock
              sent_len <- let updatedReq = (prependHeader req (generateVia localAddr transport branchByteStr)) in
                          let remote = destination direction in
                          let bytes = toByteString (requestBuilder2 updatedReq) in
                          sendTo sock bytes remote
              return (updateTransport (\_ -> updatedTransportLayer) stackWithStoredTransaction)
    where
       (generatedBranch, newGenVal) = nextVal (generatorVal stack)
       branchByteStr = packThem generatedBranch
       clientTranKey = ClientTranKey (reqMethod req) branchByteStr
       clientTran = initClientTran req clientCtx
       updatedClientTranMap = Map.insert clientTranKey clientTran (clientTranMap stack)
       stackWithStoredTransaction = updateClientTranMap updatedClientTranMap stack
       generateVia localAddr transport branch = ((BS.pack "Via"), ViaHeader (ViaValue defaultProtoVersion transport (getHost localAddr) (getPort localAddr) ((BS.pack "branch", branch):[])))

