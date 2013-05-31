module Hispania.Stack ( 
  Stack(..), newStack, maybeUpdateTransport,
  RequestHandler, ResponseHandler, ServerContext, ClientContext(..), serveOne, serveLoop,
  StackAction(..),
  inNewSession,
  SipAction(..),
  respond, createResponseTo,
  createRequestPure, sendReq, createUAContext, createClientContext
) where

import Hispania.Types
import Hispania.Parser
import Hispania.Transport
import Hispania.Util

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSW
import qualified Data.Map as Map
import Data.Word
import Data.Maybe
import qualified Data.Attoparsec.ByteString as ATP
import qualified Data.Attoparsec.ByteString.Char8 as ATPC

import Blaze.ByteString.Builder
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD
import System.IO


import Control.Monad.State
import Control.Monad.Reader

echoPort = 9900

type ClientTranMap = Map.Map ClientTranKey SipSession

type ServerTranMap = Map.Map ServerTranKey SipSession

type DialogMap = Map.Map DialogKey SipSession

type ProxyMap = Map.Map ProxyKey SipSession



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



newStack :: RequestHandler -> Stack
newStack reqHandler = Stack clearTransportLayer reqHandler 0 Map.empty Map.empty Map.empty Map.empty


data Dialog = Dialog DialogState UAContext

data DialogState = DSEarly | DSEstablished | DSTerminated

data Proxy = Proxy



data ClientTransaction = InviteClientTransaction InviteClientTranState | NonInviteClientTransaction NonInviteClientTranState

data InviteClientTranState = ICInitial | ICCalling | ICProceeding | ICCompleted | ICTerminated

data NonInviteClientTranState = NICInitial | NICTrying | NICProceeding | NICCompleted | NICTerminated

data ServerTransaction = InviteServerTransaction InviteServerTranState | NonInviteServerTransaction NonInviteServerTranState

data NonInviteServerTranState = NISInitial | NISTrying | NISProceeding | NISCompleted | NISTerminated

data InviteServerTranState = ISInitial | ISProceeding | ISCompleted | ISConfirmed | ISTerminated

isTerminatedState:: ServerTransaction -> Bool
isTerminatedState (InviteServerTransaction ISTerminated)      = True
isTerminatedState (InviteServerTransaction _ )                = False
isTerminatedState (NonInviteServerTransaction NISTerminated ) = True
isTerminatedState (NonInviteServerTransaction _)              = False



newtype ServerCtxRef = ServerCtxRef String deriving (Eq, Ord)

newtype ClientCtxRef = ClientCtxRef String deriving (Eq, Ord)

type ContextRef = Maybe (Either ServerCtxRef ClientCtxRef)

newtype UACtxRef = UACtxRef String deriving (Eq, Ord)

newtype DialogRef = DialogRef String deriving (Eq, Ord)



data ClientContext = ClientContext {remoteAddr::SipAddress, clientUA::UACtxRef, responseHandler::ResponseHandler, clientTranKey::ClientTranKey}

data ServerContext = ServerContext {servingDirection::Direction, servingTransport::Transport, serverTranKey::ServerTranKey}



data UAContext = UAContext {
                     localAddr::SipAddress, 
                     callIdVal::BS.ByteString, 
                     uaRequestHandler::RequestHandler, 
                     initialContext::ContextRef, 
                     terminatingContext:: ContextRef
                   }





type StackTask a = Stack -> IO (a, Stack)

newtype StackAction a = StackAction{runStackAction :: StackTask a}

instance Monad StackAction where
  return x = StackAction (\stack -> return (x, stack))
  k >>= b  = StackAction (\stack -> do
                                     (v, stackV) <- (runStackAction k) stack
                                     (runStackAction (b v)) stackV
                             )








type ServerCtxStore = Map.Map ServerCtxRef ServerContext

type ClientCtxStore = Map.Map ClientCtxRef ClientContext

type DialogStore = Map.Map DialogRef Dialog

type ServerTranStore = Map.Map ServerTranKey ServerTransaction

type ClientTranStore = Map.Map ClientTranKey ClientTransaction

type UACtxStore = Map.Map UACtxRef UAContext

type ErrorHandler = Stack -> IO Stack

data SipSession = SipSession {serverCtxStore::ServerCtxStore, serverTranStore::ServerTranStore, clientCtxStore::ClientCtxStore, clientTranStore::ClientTranStore, dialogStore::DialogStore, uaCtxStore::UACtxStore, errorHandler::ErrorHandler}


getServerCtx :: ServerCtxRef -> SipSession -> Maybe ServerContext
getServerCtx ref session = Map.lookup ref (serverCtxStore session) 

updateServerCtx :: ServerCtxRef -> ServerContext -> SipSession -> SipSession
updateServerCtx ref ctx session = session{serverCtxStore = (Map.insert ref ctx (serverCtxStore session))}

getServerTran :: ServerTranKey -> SipSession -> Maybe ServerTransaction
getServerTran ref session = Map.lookup ref (serverTranStore session) 

updateServerTran :: ServerTranKey -> ServerTransaction -> SipSession -> SipSession
updateServerTran ref tran session = session{serverTranStore = (Map.insert ref tran (serverTranStore session))}

removeServerTran :: ServerTranKey -> SipSession -> SipSession
removeServerTran ref session = session{serverTranStore = (Map.delete ref (serverTranStore session))}

getClientCtx :: ClientCtxRef -> SipSession -> Maybe ClientContext
getClientCtx ref session = Map.lookup ref (clientCtxStore session) 

updateClientCtx :: ClientCtxRef -> ClientContext -> SipSession -> SipSession
updateClientCtx ref ctx session = session{clientCtxStore = (Map.insert ref ctx (clientCtxStore session))}

getClientTran :: ClientTranKey -> SipSession -> Maybe ClientTransaction
getClientTran ref session = Map.lookup ref (clientTranStore session) 

updateClientTran :: ClientTranKey -> ClientTransaction -> SipSession -> SipSession
updateClientTran ref tran session = session{clientTranStore = (Map.insert ref tran (clientTranStore session))}

removeClientTran :: ClientTranKey -> SipSession -> SipSession
removeClientTran ref session = session{clientTranStore = (Map.delete ref (clientTranStore session))}

getDialog :: DialogRef -> SipSession -> Maybe Dialog
getDialog ref session = Map.lookup ref (dialogStore session)

updateDialog :: DialogRef -> Dialog -> SipSession -> SipSession
updateDialog ref dialog session = session{dialogStore = (Map.insert ref dialog (dialogStore session))}

removeDialog :: DialogRef -> SipSession -> SipSession
removeDialog ref session = session{dialogStore = (Map.delete ref (dialogStore session))}

getUACtx :: UACtxRef -> SipSession -> Maybe UAContext
getUACtx ref session = Map.lookup ref (uaCtxStore session)

updateUACtx :: UACtxRef -> UAContext -> SipSession -> SipSession
updateUACtx ref ctx session = session{uaCtxStore = (Map.insert ref ctx (uaCtxStore session))}

removeUACtx :: UACtxRef -> SipSession -> SipSession
removeUACtx ref session = session{uaCtxStore = (Map.delete ref (uaCtxStore session))}


generateServerCtxRef :: ServerTranKey -> ServerCtxRef
generateServerCtxRef tranKey = ServerCtxRef (show tranKey)


generateClientCtxRef :: ClientTranKey -> ClientCtxRef
generateClientCtxRef tranKey = ClientCtxRef (show tranKey)

generateUACtxRef :: String -> String -> UACtxRef
generateUACtxRef tag callId = UACtxRef (tag ++ callId)


defaultErrorHandler stack = return stack

newSession :: SipSession

newSession = SipSession Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty defaultErrorHandler


type SessionTask a = SipSession -> Stack -> IO (a, SipSession, Stack)




updateSession :: SipSession -> Stack -> Stack
updateSession session stack = stack{clientTranMap = updatedClientTranMap, serverTranMap = updatedServerTranMap}
     where
        clientTranKeys = (Map.keys . clientTranStore) session
        updatedClientTranMap = foldl (\map key -> Map.insert key session map) (clientTranMap stack) clientTranKeys
        serverTranKeys = (Map.keys . serverTranStore) session
        updatedServerTranMap = foldl (\map key -> Map.insert key session map) (serverTranMap stack) serverTranKeys

{-
updateSessionTask :: SessionTask
updateSessionTask updatedSession stack = let updatedStack = (updateSession updatedSession stack) 
                                         in (return (updatedSession, updatedStack))


setErrorHandler :: ErrorHandler -> SessionTask
setErrorHandler newErrorHandler session = let updatedSession = session{errorHandler = newErrorHandler}
                                          in (updateSessionTask updatedSession)
-}

serveLoop :: Stack -> IO ()
serveLoop origStack = do
           (unit, updatedStack) <- serveOne origStack
           serveLoop updatedStack

serveOne :: StackTask ()
serveOne origStack = do
           (mesg, direction, transport) <- receive (transportLayer origStack)
           putStrLn ("Received message " ++ (show direction))
           (unit, newStack) <- handle mesg direction transport origStack 
           hFlush stdout
           return ((), newStack)





inNewSession :: SessionTask a -> StackTask a
inNewSession sessionTask = \stack -> do
                                        (val, updatedSession, updatedStack) <- sessionTask newSession stack
                                        return (val, updatedStack)


handle :: BS.ByteString ->  Direction -> Transport -> StackTask ()
handle bytes direction transport stack = case parseResult of
                                           ATPC.Done _ (Right request) -> handleRequest request direction transport stack
                                           ATPC.Done _ (Left response) -> handleResponse response stack
                                           ATPC.Fail _ _ errorStr -> error  ("Failed to parse:" ++ errorStr)
                                           ATPC.Partial _ -> error "Parsed only partially"
                       where
                         parseResult = ATPC.parse messageParser bytes

createSessionForServer serverTranKey serverTran serverCtxRef serverCtx = (\sess -> sess{serverTranStore = (Map.insert serverTranKey serverTran (serverTranStore sess)), serverCtxStore = (Map.insert serverCtxRef serverCtx (serverCtxStore sess))}) newSession


handleRequest :: Request () -> Direction -> Transport -> StackTask ()
handleRequest request direction transport stack = 
     case (getServerTranKey request) of
       Nothing            -> return ((), stack)
       Just serverTranKey -> case (Map.lookup serverTranKey (serverTranMap stack)) of 
                               Just session -> case (getServerTran serverTranKey session) of
                                                  Nothing -> return ((), stack)
                                                  Just serverTran -> serverTranSMProcessReq serverTran request stack
                               Nothing      -> case (getDialogKeyFromReq request) of
                                                  Just dialogKey -> case (Map.lookup dialogKey (dialogMap stack)) of
                                                                      Nothing     -> return ((), stack)
                                                                      Just session -> return ((), stack)
                                                  Nothing ->  do
                                                                (unit, aSession, aStack) <- task
                                                                return (unit, aStack)
                                                              where
                                                                  task = reqHandler request serverCtxRef updatedSession (updateStack updatedSession stack)
                                                                  serverCtx = ServerContext direction transport serverTranKey
                                                                  reqHandler = initialRequestHandler stack
                                                                  updatedSession = createSessionForServer serverTranKey createdServerTran serverCtxRef serverCtx
                                                                  updateStack sess st = st{serverTranMap = (Map.insert serverTranKey sess (serverTranMap st))}
                                                where
                                                    serverCtxRef = generateServerCtxRef serverTranKey
                                                    createdServerTran = case (reqMethod request) of
                                                                          INVITE -> InviteServerTransaction ISInitial
                                                                          _      -> NonInviteServerTransaction NISInitial




serverTranSMProcessReq :: ServerTransaction -> Request () -> StackTask ()

serverTranSMProcessReq (NonInviteServerTransaction state ) req stack = 
    case state of
      NISInitial    -> putStrLn "retransmit" >> return ((), stack)
      NISTrying     -> putStrLn "retransmit" >> return ((), stack)
      NISProceeding -> putStrLn "retransmit" >> return ((), stack)
      NISCompleted  -> putStrLn "retransmit" >> return ((), stack)
      NISTerminated -> putStrLn "retransmit" >> return ((), stack)

serverTranSMProcessReq (InviteServerTransaction state ) req stack = 
    case state of
      ISInitial    -> putStrLn "retransmit" >> return ((), stack)
      ISProceeding -> putStrLn "retransmit" >> return ((), stack)
      ISCompleted  -> putStrLn "retransmit" >> return ((), stack)
      ISConfirmed  -> putStrLn "retransmit" >> return ((), stack)
      ISTerminated -> putStrLn "retransmit" >> return ((), stack)



handleResponse :: Response () -> StackTask ()
handleResponse res stack = 
    case (getClientTranKey res) of
       Nothing      -> return ((), stack)
       Just tranKey -> case (Map.lookup tranKey (clientTranMap stack)) of
                         Nothing      -> return ((), stack)
                         Just session -> let clientCtxRef = generateClientCtxRef tranKey in
                                         case (getClientTran tranKey session) of
                                           Nothing         -> return ((), stack)
                                           Just clientTran -> case (clientTranSMProcessRes clientTran res) of
                                                                (newClientTran, Ignore)              -> return ((), stack)
                                                                (newClientTran, HandleToApplication) -> case (getClientCtx clientCtxRef session) of
                                                                                                          Nothing        -> return ((), stack)
                                                                                                          Just clientCtx -> do
                                                                                                                              (unit, updatedSession, updatedStack) <- (responseHandler clientCtx) res clientCtxRef session stack
                                                                                                                              return (unit, updatedStack)

data IncomingResponseAction = HandleToApplication | Ignore

clientTranSMProcessRes :: ClientTransaction -> Response () -> (Maybe ClientTransaction, IncomingResponseAction)

clientTranSMProcessRes (NonInviteClientTransaction state ) res = 
    case state of
      NICInitial    -> (Just (NonInviteClientTransaction NICTrying),     HandleToApplication)
      NICTrying     -> (Just (NonInviteClientTransaction NICProceeding), HandleToApplication)
      NICProceeding -> (Just (NonInviteClientTransaction NICCompleted),  HandleToApplication)
      NICCompleted  -> (Nothing, Ignore)
      NICTerminated -> (Nothing, Ignore)

clientTranSMProcessRes (InviteClientTransaction state ) res = 
    case state of
      ICInitial    -> (Just (InviteClientTransaction ICCalling), HandleToApplication)
      ICCalling    -> (Just (InviteClientTransaction ICProceeding), HandleToApplication)
      ICProceeding -> (Just (InviteClientTransaction ICCompleted), HandleToApplication)
      ICCompleted  -> (Nothing, Ignore)
      ICTerminated -> (Nothing, Ignore)



type RequestHandler = Request () -> ServerCtxRef -> SessionTask ()

type ResponseHandler = Response () -> ClientCtxRef -> SessionTask ()


newtype SipAction a = SipAction{runSipAction :: SessionTask a}

instance Monad SipAction where
  return x = SipAction (\sess stack -> return (x, sess, stack))
  k >>= b  = SipAction (\sess stack -> do
                                             (v, sessV, stackV) <- (runSipAction k) sess stack
                                             (runSipAction (b v)) sessV stackV
                             )
  


updateAndPerform :: (SipSession, StackTask a) -> Stack -> IO (a, SipSession, Stack)
updateAndPerform (session, task) stack = do
                                           (val, updatedStack) <- task (updateSession session stack)
                                           return (val, session, updatedStack)


respond :: ServerCtxRef -> Response () -> SessionTask ()
respond ref resp session = case (sessionAction session) of
                            Nothing -> \stack -> return ((), session, stack)
                            (Just v)  -> updateAndPerform v

    where
       sessionAction :: SipSession -> Maybe (SipSession, (Stack -> IO ((), Stack)))
       sessionAction = runReaderT $ do
                                      serverCtx <- ReaderT $ getServerCtx ref
                                      let transport = servingTransport serverCtx
                                      let respDirection = calcDirectionForResponse transport (servingDirection serverCtx)
                                      let sendTask s = do
                                                        mbUpdatedTransportLayer <- sendResponseTo resp respDirection transport (transportLayer s)
                                                        return ((), (maybeUpdateTransport mbUpdatedTransportLayer s))
                                      let tranKey = serverTranKey serverCtx
                                      serverTran <- ReaderT $ getServerTran tranKey
                                      let (mbTranUpdate, responseCommand) = serverTranSMProcessRes serverTran resp
                                      let serverTranAction = maybe id (\trn -> if (isTerminatedState trn) then (removeServerTran tranKey) else (updateServerTran tranKey trn)) mbTranUpdate
                                      updatedSession <- reader serverTranAction
                                      let responseAction = case responseCommand of
                                                             SendResponseOnce -> sendTask
                                                             ResponseError    -> return . ((,) ())
                                                             SkipResponse     -> return . ((,) ())
                                      return (updatedSession, responseAction)



data OutgoingResponseAction = SendResponseOnce | SendResponsePeriodically | SkipResponse | ResponseError

serverTranSMProcessRes :: ServerTransaction -> Response () -> (Maybe ServerTransaction, OutgoingResponseAction)

serverTranSMProcessRes (NonInviteServerTransaction state) res = 
    case state of
      NISInitial    -> ( Just (NonInviteServerTransaction NISTrying), SendResponseOnce)
      NISTrying     -> ( Just (NonInviteServerTransaction NISProceeding), SendResponseOnce)
      NISProceeding -> ( Just (NonInviteServerTransaction NISCompleted), SendResponseOnce)
      NISCompleted  -> ( Nothing, ResponseError )
      NISTerminated -> ( Nothing, ResponseError )

serverTranSMProcessRes (InviteServerTransaction state) res = 
    case state of
      ISInitial    -> ( Just (InviteServerTransaction ISProceeding), SendResponseOnce)
      ISProceeding -> ( Just (InviteServerTransaction ISTerminated), SendResponseOnce)
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


createResponseTo :: Request () -> Int -> Response ()
createResponseTo req status = Response defaultProtoVersion status (defaultReason status) (copyFromRequest (reqHeaders req)) ()
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
getUriPort uri = case uri of 
                    RawURI _ _              -> PortNum (fromIntegral 5060)
                    SipURI _ _ _ _ mbPort _ -> case mbPort of
                                                   Nothing   -> PortNum (fromIntegral 5060)
                                                   Just port -> PortNum (port)

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



createRequestPure :: RequestMethod -> URI -> URI -> Stack -> (Request (), Stack)
createRequestPure method from to stack = (Request method to defaultProtoVersion headers (), updateGenerator newGenVal stack)
  where
     ((callId, branch, localTag), newGenVal) = runState ( let stateGen = state nextVal in liftM3 (,,) stateGen stateGen stateGen )  (generatorVal stack)
     callIdHdr = ((BS.pack "Call-ID"), (GenericHeader (packThem callId)))
     fromHdr   = ((BS.pack "From") , (GenericHeader (packThem localTag)))
     cseqHdr   = ((BS.pack "CSeq"), (GenericHeader (BS.pack "1")))
     headers   = callIdHdr : fromHdr : cseqHdr : []


createUAContextPure:: SipAddress -> RequestHandler -> SipSession -> Stack -> (UACtxRef, SipSession, Stack)
createUAContextPure local reqH session stack = (uaCtxRef, updatedSession, updatedStack)
  where
     ((callIdTmp, localTagTmp), newGenVal) = runState ( let stateGen = state nextVal in liftM2 (,) stateGen stateGen )  (generatorVal stack)    
     updatedLocal = addAddressParam (BS.pack "tag") (packThem localTagTmp) local
     uaCtxRef = generateUACtxRef (BS.unpack (packThem localTagTmp)) (BS.unpack (packThem callIdTmp))
     uaCtx = UAContext updatedLocal (packThem callIdTmp) reqH Nothing Nothing
     updatedSession = updateUACtx uaCtxRef uaCtx session
     updatedStack = ((updateGenerator newGenVal) . (updateSession updatedSession)) stack


createUAContext:: SipAddress -> RequestHandler -> SessionTask UACtxRef
createUAContext local reqH session stack = return (createUAContextPure local reqH session stack)


createClientContextPure:: SipAddress -> RequestMethod -> ResponseHandler -> UACtxRef -> SipSession -> Stack -> Maybe ((ClientCtxRef, Request ()), SipSession, Stack)
createClientContextPure remote method respHandler uaRef session stack = 
          case (getUACtx uaRef session) of
              Nothing -> Nothing
              Just ua -> Just ((clientCtxRef,request), updatedSession, updatedStack)
                           where
                             callIdHdr  = ((BS.pack "Call-ID") , GenericHeader (callIdVal ua))
                             fromHdr    = ((BS.pack "From")    , AddressHeader (localAddr ua))
                             cseqHdr    = ((BS.pack "CSeq")    , GenericHeader  (BS.pack "1"))
                             toHdr      = ((BS.pack "To")      , AddressHeader remote)
                             (generatedBranch, newGenVal) = nextVal (generatorVal stack)
                             branchByteStr = packThem generatedBranch
                             tranKey    = ClientTranKey method branchByteStr
                             headers    = callIdHdr : fromHdr : cseqHdr : toHdr : []
                             requestURI = addrURI remote
                             request = Request method requestURI defaultProtoVersion headers ()
                             clientCtx = ClientContext remote uaRef respHandler tranKey
                             clientCtxRef = generateClientCtxRef tranKey
                             updatedSession = updateClientCtx clientCtxRef clientCtx session
                             updatedStack = ((updateGenerator newGenVal) . (updateSession updatedSession)) stack

createClientContext:: SipAddress -> RequestMethod -> ResponseHandler -> UACtxRef -> SessionTask (ClientCtxRef, Request ())
createClientContext remote method respHandler uaRef session stack = maybe (fail "error") return (createClientContextPure remote method respHandler uaRef session stack)


initClientTran :: Request a -> ClientTransaction
initClientTran req = 
   case (reqMethod req) of
     INVITE -> InviteClientTransaction ICInitial
     _      -> NonInviteClientTransaction NICInitial



sendReq :: Request () -> ClientCtxRef -> SessionTask ()
sendReq req clientRef session stack = 
   case (getClientCtx clientRef session) of
     Nothing        -> return ((), session, stack)
     Just clientCtx -> updateAndPerform (updatedSession, sendReqTask) stack
                         where
                           clientTran = initClientTran req
                           tranKey = clientTranKey clientCtx
                           updatedSession = updateClientTran tranKey clientTran session
                           generateVia localAddr transport = ((BS.pack "Via"), ViaHeader (ViaValue defaultProtoVersion transport (getHost localAddr) (getPort localAddr) ((BS.pack "branch", (clientTranBranch tranKey)):[])))
                           sendReqTask = \stack -> do
                                                    putStrLn ("Request:" ++ (show req))
                                                    (direction, transport) <- getDirection req
                                                    (sock, mbUpdatedTransportLayer) <- getSocket transport direction (transportLayer stack)
                                                    localAddr <- getSocketName sock
                                                    sent_len <- let updatedReq = (prependHeader req (generateVia localAddr transport)) in
                                                                let remote = destination direction in
                                                                let bytes = toByteString (requestBuilder2 updatedReq) in
                                                                sendTo sock bytes remote >> (putStrLn ("Sent some bytes to:" ++ (show remote)))

                                                    let updatedStack = maybeUpdateTransport mbUpdatedTransportLayer stack
                                                    return ((), updatedStack)
