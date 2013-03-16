module Hispania.Stack ( 
  Stack(..), newStack, maybeUpdateTransport,
  RequestHandler, ResponseHandler, ServerContext, ClientContext(..), serveOne, serveLoop,
  createRequest, sendRequest, sendReq, respond, respondTo, createUAContext, createClientContext
) where

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
import System.IO


import Control.Monad.State
import Data.Word

import Data.Maybe

echoPort = 9900

data Stack = Stack {
       transportLayer::TransportLayer, 
       initialRequestHandler::RequestHandler, 
       generatorVal::Word, 
       clientTranMap :: ClientTranMap, 
       serverTranMap :: ServerTranMap, 
       dialogMap :: DialogMap,
       proxyMap :: ProxyMap
}


maybeUpdateTransport :: (Maybe TransportLayer) -> Stack -> Stack
maybeUpdateTransport mbUpdatedTransportLayer stack = maybe stack (\tl -> stack{transportLayer = tl}) mbUpdatedTransportLayer

updateGenerator :: Word -> Stack -> Stack
updateGenerator gen stack = (liftM7 Stack transportLayer initialRequestHandler (return gen) clientTranMap serverTranMap dialogMap proxyMap) stack

updateClientTran :: (ClientTranKey, ClientTransaction) -> Stack -> Stack
updateClientTran (key, val) stack = (liftM7 Stack transportLayer initialRequestHandler generatorVal ((Map.insert key val) . clientTranMap) serverTranMap dialogMap proxyMap) stack

updateClientTranMap :: ClientTranMap -> Stack -> Stack
updateClientTranMap updatedTranMap stack = stack{clientTranMap=updatedTranMap}


insertServerTran :: ServerTranKey -> ServerTransaction -> Stack -> Stack
insertServerTran key tran stack = let updatedServerTranMap = Map.insert key tran (serverTranMap stack) in
                                  stack{serverTranMap=updatedServerTranMap}


removeServerTran :: ServerTranKey -> Stack -> Stack
removeServerTran key stack = let updatedServerTranMap = Map.delete key (serverTranMap stack) in
                             stack{serverTranMap = updatedServerTranMap}


newStack :: RequestHandler -> Stack
newStack reqHandler = Stack clearTransportLayer reqHandler 0 Map.empty Map.empty Map.empty Map.empty

type ClientTranMap = Map.Map ClientTranKey ClientTransaction

type ServerTranMap = Map.Map ServerTranKey ServerTransaction

type DialogMap = Map.Map DialogKey Dialog

type ProxyMap = Map.Map ProxyKey Proxy

data Dialog = Dialog

data Proxy = Proxy

-- class Context a where
--   extractStack :: a -> Stack

-- instance Context ServerContext where
--   extractStack (ServerContext stack _ _ _ _) = stack





data ClientTransaction = InviteClientTransaction InviteClientTranState ClientContext | NonInviteClientTransaction NonInviteClientTranState ClientContext

data InviteClientTranState = ICInitial | ICCalling | ICProceeding | ICCompleted | ICTerminated

data NonInviteClientTranState = NICInitial | NICTrying | NICProceeding | NICCompleted | NICTerminated

data ServerTransaction = InviteServerTransaction InviteServerTranState ServerContext | NonInviteServerTransaction NonInviteServerTranState ServerContext

data NonInviteServerTranState = NISInitial | NISTrying | NISProceeding | NISCompleted | NISTerminated

data InviteServerTranState = ISInitial | ISProceeding | ISCompleted | ISConfirmed | ISTerminated

isTerminatedState:: ServerTransaction -> Bool
isTerminatedState (InviteServerTransaction ISTerminated _)     = True
isTerminatedState (InviteServerTransaction _ _)                = False
isTerminatedState (NonInviteServerTransaction NISTerminated _) = True
isTerminatedState (NonInviteServerTransaction _ _)             = False

getClientContext :: ClientTransaction -> ClientContext
getClientContext (InviteClientTransaction state clientCtx) = clientCtx
getClientContext (NonInviteClientTransaction state clientCtx) = clientCtx


data ClientContext = ClientContext {remoteAddr::SipAddress, clientUA::UAContext, responseHandler::ResponseHandler, requestPrototype::Request ()}

data ServerContext = ServerContext {servingStack::Stack, servingDirection::Direction, servingTransport::Transport, servingHandler::RequestHandler, serverTran::ServerTransaction, serverTranKey::ServerTranKey}


data DialogKey = DialogKey BS.ByteString BS.ByteString BS.ByteString

data ProxyKey = ProxyKey BS.ByteString



data UAContext = UAContext {localAddr::SipAddress, callIdVal::BS.ByteString}

type ResponseHandler = Request () -> ()



type StackTask = Stack -> IO Stack


serveLoop :: Stack -> IO ()
serveLoop origStack = do
           updatedStack <- serveOne origStack
           serveLoop updatedStack

serveOne :: StackTask
serveOne origStack = do
           (mesg, direction, transport) <- receive (transportLayer origStack)
           putStrLn ("Received message " ++ (show direction))
           newStack <- handle mesg direction transport origStack 
           hFlush stdout
           return newStack



handle :: BS.ByteString ->  Direction -> Transport -> StackTask
handle bytes direction transport stack = case parseResult of
                                           ATPC.Done _ (Right request) -> handleRequest request direction transport stack
                                           ATPC.Done _ (Left response) -> handleResponse response stack
                                           ATPC.Fail _ _ errorStr -> error  ("Failed to parse:" ++ errorStr)
                                           ATPC.Partial _ -> error "Parsed only partially"
                       where
                         parseResult = ATPC.parse messageParser bytes


handleRequest :: Request () -> Direction -> Transport -> StackTask
handleRequest request direction transport stack = 
     case (getServerTranKey request) of
       Nothing            -> return stack
       Just serverTranKey -> case (Map.lookup serverTranKey (serverTranMap stack)) of 
                               Nothing         -> reqHandler request serverCtx (insertServerTran serverTranKey createdServerTran stack)
                                                    where
                                                       serverCtx = ServerContext stack direction transport reqHandler createdServerTran serverTranKey
                                                       reqHandler = initialRequestHandler stack
                                                       createdServerTran = case (reqMethod request) of
                                                                             INVITE -> InviteServerTransaction ISInitial serverCtx
                                                                             _      -> NonInviteServerTransaction NISInitial serverCtx 

                               Just serverTran -> serverTranSMProcessReq serverTran request stack

serverTranSMProcessReq :: ServerTransaction -> Request () -> StackTask

serverTranSMProcessReq (NonInviteServerTransaction state _) req stack = 
    case state of
      NISInitial    -> putStrLn "retransmit" >> return stack
      NISTrying     -> putStrLn "retransmit" >> return stack
      NISProceeding -> putStrLn "retransmit" >> return stack
      NISCompleted  -> putStrLn "retransmit" >> return stack
      NISTerminated -> putStrLn "retransmit" >> return stack

serverTranSMProcessReq (InviteServerTransaction state _) req stack = 
    case state of
      ISInitial    -> putStrLn "retransmit" >> return stack
      ISProceeding -> putStrLn "retransmit" >> return stack
      ISCompleted  -> putStrLn "retransmit" >> return stack
      ISConfirmed  -> putStrLn "retransmit" >> return stack
      ISTerminated -> putStrLn "retransmit" >> return stack

handleResponse :: Response () -> Stack -> IO Stack
handleResponse res stack = case clientTranResult of
                             Nothing -> return stack
                             Just clientTran -> do
                                                 return (responseHandler (getClientContext clientTran))
                                                 return stack
  where
    clientTranResult = getClientTranKey res >>= (\key -> Map.lookup key (clientTranMap stack))


clientTranSMProcessRes :: ClientTransaction -> Request () -> StackTask

clientTranSMProcessRes (NonInviteClientTransaction state _) req stack = 
    case state of
      NICInitial    -> return stack
      NICTrying     -> return stack
      NICProceeding -> return stack
      NICCompleted  -> return stack
      NICTerminated -> return stack

clientTranSMProcessRes (InviteClientTransaction state _) req stack = 
    case state of
      ICCalling    -> return stack
      ICProceeding -> return stack
      ICCompleted  -> return stack
      ICTerminated -> return stack


type RequestHandler = Request () -> ServerContext -> StackTask

respond :: ServerContext -> Response () -> StackTask
respond serverCtx response stack = case responseAction of
                                     SendResponseOnce -> sendTask stack >>= (return . updateServerTran)
                                     ResponseError    -> (return . updateServerTran) stack
                                     SkipResponse     -> (return . updateServerTran) stack
   where
      (ServerContext stack direction transport _ tran key) = serverCtx
      respDirection = calcDirectionForResponse transport direction
      (mbTranUpdate, responseAction) = serverTranSMProcessRes tran response
      sendTask s = do
                     mbUpdatedTransportLayer <- sendResponseTo response respDirection transport (transportLayer s)
                     return (maybeUpdateTransport mbUpdatedTransportLayer s)
      updateServerTran = maybe id (\trn -> if (isTerminatedState trn) then (removeServerTran key) else (insertServerTran key trn)) mbTranUpdate
                                       


data ResponseAction = SendResponseOnce | SendResponsePeriodically | SkipResponse | ResponseError

serverTranSMProcessRes :: ServerTransaction -> Response () -> (Maybe ServerTransaction, ResponseAction)

serverTranSMProcessRes (NonInviteServerTransaction state serverCtx) res = 
    case state of
      NISInitial    -> ( Just (NonInviteServerTransaction NISTrying serverCtx), SendResponseOnce)
      NISTrying     -> ( Just (NonInviteServerTransaction NISProceeding serverCtx), SendResponseOnce)
      NISProceeding -> ( Just (NonInviteServerTransaction NISCompleted serverCtx), SendResponseOnce)
      NISCompleted  -> ( Nothing, ResponseError )
      NISTerminated -> ( Nothing, ResponseError )

serverTranSMProcessRes (InviteServerTransaction state serverCtx) res = 
    case state of
      ISInitial    -> ( Just (InviteServerTransaction ISProceeding serverCtx), SendResponseOnce)
      ISProceeding -> ( Just (InviteServerTransaction ISTerminated serverCtx), SendResponseOnce)
      ISCompleted  -> ( Nothing, ResponseError )
      ISConfirmed  -> ( Nothing, ResponseError )
      ISTerminated -> ( Nothing, ResponseError )




calcDirectionForResponse :: Transport -> Direction -> Direction
calcDirectionForResponse TCP reqDirection = reverseDirection reqDirection
calcDirectionForResponse UDP reqDirection = Direction (fromJust (source reqDirection), Nothing)

sendRequestTo :: Request () -> Direction -> Transport -> TransportLayer -> IO (Maybe TransportLayer)
sendRequestTo =  sendRawTo . toByteString . requestBuilder2

sendResponseTo :: Response () -> Direction -> Transport -> TransportLayer -> IO (Maybe TransportLayer)
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
              mbUpdatedTransportLayer <- sendRequestTo req direction transport (transportLayer stack)
              return (maybeUpdateTransport mbUpdatedTransportLayer stack)


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
              (sock, mbUpdatedTransportLayer) <- getSocket transport direction (transportLayer stack)
              localAddr <- getSocketName sock
              sent_len <- let updatedReq = (prependHeader req (generateVia localAddr transport branchByteStr)) in
                          let remote = destination direction in
                          let bytes = toByteString (requestBuilder2 updatedReq) in
                          sendTo sock bytes remote
              return (maybeUpdateTransport mbUpdatedTransportLayer stackWithStoredTransaction)
    where
       (generatedBranch, newGenVal) = nextVal (generatorVal stack)
       branchByteStr = packThem generatedBranch
       clientTranKey = ClientTranKey (reqMethod req) branchByteStr
       clientTran = initClientTran req clientCtx
       updatedClientTranMap = Map.insert clientTranKey clientTran (clientTranMap stack)
       stackWithStoredTransaction = updateClientTranMap updatedClientTranMap stack
       generateVia localAddr transport branch = ((BS.pack "Via"), ViaHeader (ViaValue defaultProtoVersion transport (getHost localAddr) (getPort localAddr) ((BS.pack "branch", branch):[])))

