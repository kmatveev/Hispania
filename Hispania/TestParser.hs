module Hispania.TestParser where

import Hispania.Types
import Hispania.Parser
import Hispania.TestTypes

import qualified Data.ByteString.Char8 as BS
import Blaze.ByteString.Builder
import Data.Attoparsec.ByteString

testHeadersStr = "From: Papa <sip:papa@home>\r\nTo: Mama <sip:mama@home>\r\nCall-ID: 1\r\n"
testReqStr = BS.pack ("INVITE sip:user@host SIP_2_0\r\n" ++ testHeadersStr ++ "\r\nbody")

parsedTestReq = parseOnly requestParser testReqStr

parsedTestMsg = parse messageParser testReqStr

reqBytes :: BS.ByteString
reqBytes = (toByteString (requestBuilder testReq) )

testViaStr = "SIP/2.0/TCP myhost:5060;branch=sdfhkjfqwefhlqwe"

parsedVia = parseOnly viaParser (BS.pack testViaStr)

testFromStr = "From: kmatveev <sip:kmatveev@domain>;tag=djfhwkjeh"

testCSeqStr = "CSeq: 1 INVITE"

testParamsStr = ";paramName=paramVal;nnn=vvv;a=b"
parsedParams = parseOnly paramsParser (BS.pack testParamsStr)

emptyParamsStr = ""
parsedEmptyParams = parseOnly paramsParser (BS.pack emptyParamsStr)

testRespStr = BS.pack ("SIP/2.0 200 OK\r\n" ++ testHeadersStr ++ "\r\n")
parsedTestResp = parseOnly responseParser testRespStr


testViaComposeAndParse = (origVia, viaByteStr, parsedVia )
  where
     origVia = ViaHeader (ViaValue defaultProtoVersion UDP (BS.pack "192.168.0.1") 5060 ((BS.pack "branch",BS.pack "ad12rcwef"):[]))
     viaByteStr = toByteString (headerValBuilder origVia)
     parsedVia = parseOnly viaParser viaByteStr


parsedViaHeader = parseOnly headerParser (BS.pack ("Via :" ++ testViaStr))


parsedSipURI = parseOnly uriParser (BS.pack "sip:kmatveev@host:5060;transport=tcp;play=forever")

parsedTelURI = parseOnly uriParser (BS.pack "tel:3472309342730")

parsedAddressA = parseOnly addressParser (BS.pack "kmatveev <sip:kmatveev@host:5060;transport=tcp;play=forever>;tag=234214")

parsedAddressB = parseOnly addressParser (BS.pack "\"Konstantin Matveev\" <sip:kmatveev@host:5060;transport=tcp;play=forever>;tag=234214")

parsedAddressC = parseOnly addressParser (BS.pack "kmatveev <tel:+72324234242>")

parsedAddressD = parseOnly addressParser (BS.pack "sip:kmatveev@host:5060;transport=tcp;play=forever")

parsedAddressE = parseOnly addressParser (BS.pack "<sip:kmatveev@host:5060;transport=tcp;play=forever>")

-- parsedFromHeader = parseOnly headerParser (BS.pack "From: kmatveev <sip:kmatveev@host:5060>")


parsedFromHeader = parseOnly headerParser (BS.pack "From: Papa <sip:papa@home>")
parsedFromURI = case parsedFromHeader of
                  Right (name, header) -> Just header
                  _ -> Nothing