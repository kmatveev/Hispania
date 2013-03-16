module Hispania.Printer where

instance Show URI where
   show (RawURI schema specific) = (BS.unpack schema) ++ ":" ++ (BS.unpack specific)
   show (SipURI secure user password host port params) = (if secure then "sips:" else "sip") ++ ":" ++ (show user) ++ ":" ++ (show password) ++ "@" ++ (show host) ++ (maybe "" (\x -> ":" ++ (show x)) port) ++ ";" ++ (showParams params)

instance Show SipAddress where
  show (SipAddress displayName uri params) = (maybe "" BS.unpack displayName) ++ "<" ++ (show uri) ++ ">" ++ (showParams params)


instance Show ViaValue where
  show (ViaValue protoVersion transport host port params) = (show protoVersion) ++ "/" ++ (show transport) ++ " " ++ (show host) ++ ":" ++ (show port) ++ (showParams params)


instance Show CSeqValue where
  show (CSeqValue seqnum method) = (show seqnum) ++ " " ++ (show method)

instance Show Header where
  show (GenericHeader value) = BS.unpack value
  show (AddressHeader value) = show value
  show (IntHeader value) = show value
  show (CSeqHeader cseqVal) = show cseqVal
  show (ViaHeader viaVal) = show viaVal

instance (Show a) => Show (Request a) where
  show (Request method uri version headers body) = (show method) ++ " " ++ (show uri) ++ " " ++ (show version) ++ "\r\n" ++ (showHeaders headers) ++ (show body)

instance (Show a) => Show (Response a) where
  show (Response version status reason headers body) = (show version) ++ " " ++ (show status) ++ " " ++ (BS.unpack reason) ++ "\r\n" ++ (showHeaders headers) ++ (show body)

showHeaders :: [(BS.ByteString, Header)] -> String
showHeaders = foldl (\x y -> x ++ (BS.unpack (fst y)) ++ ":" ++ (show (snd y)) ++ "\r\n") ""

instance Show Transport where
  show TCP = "TCP"
  show UDP = "UDP"
  show SCTP = "SCTP"
  show (Unknown val) = show val

instance Show RequestMethod where
  show x = 
    case x of
      Custom c -> BS.unpack c
      _ -> fromJust (lookup x methodMapStr)

instance Show ProtoVersion where
  show (ProtoVersion proto version) = (show proto) ++ "/" ++ (show version)

showParams :: Params -> String
showParams = (foldr (++) "") . (map (\(x, y) -> (show x) ++ "=" ++ (show y)) )

