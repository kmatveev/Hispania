module Hispania.TestTypes where

import Hispania.Types
import Data.ByteString.Char8 as BS

-- testFrom = (GenericHeader (BS.pack "From") (BS.pack "<sip:from@from>"))
testFrom = ((BS.pack "From"), AddressHeader (SipAddress (Just (BS.pack "FullName")) (SipURI False (BS.pack "fromusr") BS.empty (BS.pack "fromhost") (Just 5060) []) []))

testTo = ((BS.pack "To"), (GenericHeader (BS.pack"<sip:to@to>")))

testCallId = ((BS.pack "Call-ID"), (GenericHeader (BS.pack "235205720357")))

testReq = Request INVITE (RawURI (BS.pack "sip") (BS.pack "user@host") ) defaultProtoVersion [testFrom, testTo, testCallId] (BS.pack "SDP")

topFrom = getTopHeader testReq (BS.pack "From")

topTo = getTopHeader testReq (BS.pack "To")

topCallId = getTopHeader testReq (BS.pack "Call-ID")

