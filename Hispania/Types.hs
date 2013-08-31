module Hispania.Types where

import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Word
import Control.Monad

data Transport = TCP | UDP | SCTP | TLS | Unknown BS.ByteString deriving (Eq, Ord, Show)

data RequestMethod = INVITE | ACK | BYE | CANCEL | REGISTER | OPTIONS | SUBSCRIBE | NOTIFY | MESSAGE | INFO | REFER | PUBLISH | PRACK | Custom BS.ByteString deriving (Eq, Ord, Show)

methodMapStr :: [(RequestMethod, String)]
methodMapStr = 
    [       
      (INVITE,"INVITE"),
      (ACK, "ACK"),
      (BYE, "BYE"),
      (CANCEL, "CANCEL"),
      (REGISTER, "REGISTER"),
      (OPTIONS, "OPTIONS"),
      (SUBSCRIBE, "SUBSCRIBE"),
      (NOTIFY, "NOTIFY"),
      (MESSAGE, "MESSAGE"),
      (INFO, "INFO"),
      (REFER, "REFER"),
      (PUBLISH, "PUBLISH"),
      (PRACK, "PRACK")
    ]

methodMapBstr = map (\x -> ( (fst x), (BS.pack (snd x)) ) ) methodMapStr


data ProtoVersion = ProtoVersion BS.ByteString BS.ByteString deriving (Eq, Ord, Show)


defaultProtoVersion = (ProtoVersion (BS.pack "SIP") (BS.pack "2.0") )

data URI = 
     RawURI BS.ByteString BS.ByteString 
     | SipURI {secure::Bool, uriUser::BS.ByteString, uriPassword::BS.ByteString, uriHost::BS.ByteString, uriPort:: Maybe Word16, uriParams::Params } deriving (Eq, Ord, Show)

type Params = [(BS.ByteString,BS.ByteString)]

getParam :: BS.ByteString -> Params -> Maybe BS.ByteString
getParam = lookup

addParam :: BS.ByteString -> BS.ByteString -> Params -> Params
addParam name value params = (name,value):params

data SipAddress = SipAddress { displayableName:: Maybe BS.ByteString, addrURI::URI, addrParams::Params} deriving (Eq, Ord, Show)

addAddressParam :: BS.ByteString -> BS.ByteString -> SipAddress -> SipAddress
addAddressParam name value (SipAddress displayName uri params) = SipAddress displayName uri (addParam name value params)

addURIParam :: BS.ByteString -> BS.ByteString -> URI -> URI
addURIParam name value (SipURI secure user password host port params) = SipURI secure user password host port (addParam name value params)

data ViaValue = ViaValue {viaProtoVersion::ProtoVersion, viaTransport::Transport, viaHost::BS.ByteString, viaPort::Word16, viaParams::Params} deriving (Eq, Ord, Show)

data CSeqValue = CSeqValue Int RequestMethod deriving (Eq, Ord, Show)

data Header = 
     GenericHeader BS.ByteString
     | AddressHeader SipAddress
     | CSeqHeader CSeqValue
     | IntHeader Int
     | ViaHeader ViaValue deriving (Eq, Ord, Show)

toVia :: Header -> Maybe ViaValue
toVia (ViaHeader viaVal) = Just viaVal
toVia _ = Nothing

toAddress :: Header -> Maybe SipAddress
toAddress (AddressHeader addr) = Just addr
toAddress _ = Nothing

getHeaderVal :: Header -> BS.ByteString
getHeaderVal (GenericHeader v) = v
getHeaderVal _ = BS.pack "no_val"


data Request a = 
     Request {reqMethod :: RequestMethod, reqURI :: URI, reqVersion :: ProtoVersion, reqHeaders :: [(BS.ByteString, Header)], reqBody :: a } deriving (Show)


data Response a = 
     Response { respVersion :: ProtoVersion, respStatus :: Int, respReason :: BS.ByteString, respHeaders :: [(BS.ByteString, Header)], respBody :: a } deriving (Show)

class Message a where
  getAllHeaders :: a -> [(BS.ByteString, Header)]
  setHeaders :: a -> [(BS.ByteString, Header)] -> a
  getMethod :: a -> RequestMethod

instance Message (Request a) where
  getAllHeaders = reqHeaders
  setHeaders req headers = req{reqHeaders = headers}
  getMethod = reqMethod

instance Message (Response a) where
  getAllHeaders = respHeaders
  setHeaders res headers = res{respHeaders = headers}
  getMethod res = INVITE


getTopHeader :: (Message a) => a -> BS.ByteString -> Maybe Header
getTopHeader msg name = (find ((name ==) . fst) (getAllHeaders msg)) >>= Just . snd

getHeaders :: (Message a) => a -> BS.ByteString -> [Header]
getHeaders msg name = (map snd) (filter ((name ==) . fst) (getAllHeaders msg))

prependHeader :: (Message a) => a -> (BS.ByteString, Header) -> a
prependHeader msg hdr = setHeaders msg (hdr : (getAllHeaders msg))

appendHeader :: (Message a) => a -> (BS.ByteString, Header) -> a
appendHeader msg hdr = setHeaders msg ((getAllHeaders msg) ++ (hdr : []))



defaultReason :: Int -> BS.ByteString
defaultReason 200 = BS.pack "OK"
defaultReason 100 = BS.pack "Trying"
defaultReason 180 = BS.pack "Ringing"
defaultReason 400 = BS.pack "Bad request"
defaultReason 500 = BS.pack "Internal server error"
defaultReason 302 = BS.pack "Moved temporary"    




data ClientTranKey = ClientTranKey {clientTranMethod::RequestMethod, clientTranBranch::BS.ByteString} deriving (Eq, Ord, Show)

getClientTranKey :: (Message a) => a -> Maybe ClientTranKey
getClientTranKey message = (getTopHeader message (BS.pack "Via")) >>= toVia >>= (Just . viaParams) >>= (getParam (BS.pack "branch")) >>= (Just . ClientTranKey (getMethod message))

data ServerTranKey = ServerTranKey {serverTranMethod::RequestMethod, serverTranBranch::BS.ByteString} deriving (Eq, Ord, Show)

getServerTranKey :: (Message a) => a -> Maybe ServerTranKey
getServerTranKey message = (getTopHeader message (BS.pack "Via")) >>= toVia >>= (Just . viaParams) >>= (getParam (BS.pack "branch")) >>= (Just . ServerTranKey (getMethod message))


data DialogKey = DialogKey{callId::BS.ByteString, localTag::BS.ByteString, remoteTag::BS.ByteString} deriving (Eq, Ord, Show)

getDialogKeyFromReq :: Request a -> Maybe DialogKey
getDialogKeyFromReq req = getDialogKey req True

getDialogKeyFromRes :: Response a -> Maybe DialogKey
getDialogKeyFromRes res = getDialogKey res False

getDialogKey :: (Message a) => a -> Bool -> Maybe DialogKey
getDialogKey msg reverse = if reverse then (liftM3 DialogKey mbCallId mbToTag mbFromTag) else (liftM3 DialogKey mbCallId mbFromTag mbToTag)
  where
    mbCallId = getTopHeader msg (BS.pack "Call-ID") >>= Just . getHeaderVal
    mbFromTag = getTopHeader msg (BS.pack "From") >>= toAddress >>= (Just . addrParams) >>= (getParam (BS.pack "tag"))
    mbToTag = getTopHeader msg (BS.pack "To") >>= toAddress >>= (Just . addrParams) >>= (getParam (BS.pack "tag"))


data ProxyKey = ProxyKey BS.ByteString


isDialogEstablishing :: RequestMethod -> Bool
isDialogEstablishing INVITE    = True
isDialogEstablishing SUBSCRIBE = True
isDialogEstablishing _         = False

isInDialogOnly :: RequestMethod -> Bool
isInDialogOnly BYE    = True
isInDialogOnly ACK    = True
isInDialogOnly NOTIFY = True
isInDialogOnly _      = False