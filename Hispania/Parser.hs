module Hispania.Parser where

import qualified Data.Attoparsec.ByteString as ATP
import qualified Data.Attoparsec.ByteString.Char8 as ATPC
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS
import Data.Word
import Data.Char (ord)
import Data.Maybe
import Data.Monoid
import qualified Data.Map as Map
import Hispania.Types
import Blaze.ByteString.Builder
import Control.Applicative
import Control.Monad

crlf :: BS.ByteString
crlf = BS.pack "\r\n"
sep :: BS.ByteString
sep = BS.pack ":"
space :: BS.ByteString
space = BS.pack " "

-- | Character classes

alphaClass = "A-Za-z"
digitClass = "0-9"
alphaNumClass = alphaClass ++ digitClass
tokenClass = "-+_.!%*~" ++ alphaNumClass
schemaClass = alphaNumClass ++ "+-."
reservedClass = ";?:@&=+$,/"
markClass = "-_.!~*'()"
unreservedClass = alphaNumClass ++ markClass
opaqueUriPartClass = unreservedClass ++ reservedClass
userUnreservedClass = "&=+$,;?/"
userClass = unreservedClass ++ userUnreservedClass
hostClass = alphaNumClass ++ "."

methodByteStr :: RequestMethod -> BS.ByteString
methodByteStr (Custom c) = c
methodByteStr x = fromJust (lookup x methodMapBstr)

intByteString :: Int -> BS.ByteString
intByteString = BS.pack . show 

wordByteString :: Word16 -> BS.ByteString
wordByteString = BS.pack . show 


paramBuilder :: (BS.ByteString, BS.ByteString) -> Builder
paramBuilder (name,value) = (fromByteString (BS.pack ";")) `mappend` (fromByteString name) `mappend` (fromByteString (BS.pack "=")) `mappend` (fromByteString value)

paramsBuilder :: [(BS.ByteString, BS.ByteString)] -> Builder
paramsBuilder = mconcat . (map paramBuilder)

uriBuilder :: URI -> Builder
uriBuilder (RawURI schema specific) = (fromByteString schema) `mappend` (fromByteString sep) `mappend` (fromByteString specific)
uriBuilder (SipURI secure uriUser uriPassword uriHost uriPort uriParams) = (fromByteString (BS.pack "sip")) `mappend` (fromByteString sep) `mappend` (fromByteString uriUser)  `mappend` (fromByteString (BS.pack "@")) `mappend` (fromByteString uriHost) `mappend` (maybe mempty (\x -> (fromByteString (BS.pack ":")) `mappend` (fromByteString (wordByteString x))) uriPort) `mappend` (paramsBuilder uriParams)

addressBuilder :: SipAddress -> Builder
addressBuilder (SipAddress displayName uri addressParams) = displayNamePart `mappend` langle `mappend` (uriBuilder uri) `mappend` rangle `mappend` (paramsBuilder addressParams)
   where
       displayNamePart = maybe mempty fromByteString displayName
       langle = (fromByteString (BS.pack "<"))
       rangle = (fromByteString (BS.pack ">"))

viaBuilder (ViaValue viaProtoVersion viaTransport viaHost viaPort viaParams) = (protoVersionBuilder viaProtoVersion) `mappend` (fromByteString (BS.pack "/") ) `mappend` (transportBuilder viaTransport) `mappend` (fromByteString space) `mappend` (fromByteString viaHost) `mappend` (fromByteString (BS.pack ":")) `mappend` (fromByteString (wordByteString viaPort)) `mappend` (paramsBuilder viaParams)

headerValBuilder :: Header -> Builder
headerValBuilder (GenericHeader value) = fromByteString value
headerValBuilder (AddressHeader addrValue) = addressBuilder addrValue
headerValBuilder (ViaHeader viaValue) = viaBuilder viaValue
headerValBuilder (IntHeader value) = (fromByteString (intByteString value))

headerBuilder :: (BS.ByteString, Header) -> Builder
headerBuilder header =  (fromByteString (fst header)) `mappend` (fromByteString sep) `mappend` (headerValBuilder (snd header))

headersBuilder :: [(BS.ByteString, Header)] -> Builder
headersBuilder = mconcat .  (map (`mappend` (fromByteString crlf))) . (map headerBuilder)

protoVersionBuilder :: ProtoVersion -> Builder
protoVersionBuilder (ProtoVersion proto version) = (fromByteString proto) `mappend` (fromByteString (BS.pack "/") ) `mappend` (fromByteString version)

transportBuilder :: Transport -> Builder
transportBuilder (Unknown val) = fromByteString val
transportBuilder UDP = fromByteString (BS.pack "UDP")
transportBuilder TCP = fromByteString (BS.pack "TCP")

requestBuilder :: Request BS.ByteString -> Builder
requestBuilder (Request method uri protoVersion headers body) = startLine `mappend` (headersBuilder headers) `mappend` (fromByteString crlf) `mappend` (fromByteString body)
    where
        startLine = (fromByteString (methodByteStr method)) `mappend` (fromByteString space) `mappend` (uriBuilder uri) `mappend` (fromByteString space) `mappend` (protoVersionBuilder protoVersion) `mappend` (fromByteString crlf)

requestBuilder2:: Request () -> Builder
requestBuilder2 (Request method uri protoVersion headers body) = startLine `mappend` (headersBuilder headers) `mappend` (fromByteString crlf)
    where
        startLine = (fromByteString (methodByteStr method)) `mappend` (fromByteString space) `mappend` (uriBuilder uri) `mappend` (fromByteString space) `mappend` (protoVersionBuilder protoVersion) `mappend` (fromByteString crlf)


responseBuilder :: Response BS.ByteString -> Builder
responseBuilder (Response protoVersion status reason headers body) = startLine `mappend` (headersBuilder headers) `mappend` (fromByteString crlf) `mappend` (fromByteString body)
    where
        startLine = (protoVersionBuilder protoVersion) `mappend` (fromByteString space) `mappend` (fromByteString (intByteString status)) `mappend` (fromByteString space) `mappend` (fromByteString reason) `mappend` (fromByteString crlf)

responseBuilder2 :: Response () -> Builder
responseBuilder2 (Response protoVersion status reason headers body) = startLine `mappend` (headersBuilder headers) `mappend` (fromByteString crlf)
    where
        startLine = (protoVersionBuilder protoVersion) `mappend` (fromByteString space) `mappend` (fromByteString (intByteString status)) `mappend` (fromByteString space) `mappend` (fromByteString reason) `mappend` (fromByteString crlf)

requestStartParser :: ATP.Parser(BS.ByteString, (BS.ByteString, BS.ByteString), (BS.ByteString, BS.ByteString) )
requestStartParser = do
               method <- token
               ATP.skipWhile isSpace
               uri <- rawUriParser
               ATP.skipWhile isSpace
               protocol <- token
               ATP.string (BS.pack "/")
               version <- token
               ATP.string (crlf)
               return (method, uri, (protocol, version))

rawHeaderParser :: ATP.Parser (BS.ByteString, BS.ByteString)
rawHeaderParser = do
           headerName <- token
           ATP.skipWhile isSpace
           ATP.skip (ATP.inClass ":")
           ATP.skipWhile isSpace
           headerValue <- ATP.takeWhile1 (\x -> not (ATP.inClass "\r\n" x))
           return (headerName, headerValue)

dumbHeaderParser :: ATP.Parser (BS.ByteString, Header)
dumbHeaderParser = do
           (name, value) <- rawHeaderParser
           return (name , (GenericHeader value))

headerParser :: ATP.Parser (BS.ByteString, Header)
headerParser = do
           headerName <- token
           ATP.skipWhile isSpace
           ATP.skip (ATP.inClass ":")
           ATP.skipWhile isSpace
           headerVal <- headerValueParser headerName
           return (headerName, headerVal)

headerParserMap :: Map.Map BS.ByteString (ATP.Parser Header)
headerParserMap = Map.fromList [(BS.pack "From", address), (BS.pack "To", address), (BS.pack "Via", via), (BS.pack "Contact", address)]
     where
        address = liftM AddressHeader addressParser
        via = liftM ViaHeader viaParser

headerValueParser :: BS.ByteString -> ATP.Parser Header
headerValueParser headerName = case preferredParser of
             Just prefParser -> prefParser <|> genericParser
             Nothing         -> genericParser
     where
        preferredParser = Map.lookup headerName headerParserMap
        genericParser = do
                         headerValue <- ATP.takeWhile1 (\x -> not (ATP.inClass "\r\n" x))
                         return (GenericHeader headerValue)


rawHeadersParser :: ATP.Parser [(BS.ByteString, BS.ByteString)]
rawHeadersParser = rawHeaderParser `ATP.sepBy` (ATP.string (crlf))

headersParser :: ATP.Parser [(BS.ByteString, Header)]
headersParser = headerParser `ATP.sepBy` (ATP.string (crlf))

rawRequestParser :: ATP.Parser ( (BS.ByteString, (BS.ByteString, BS.ByteString), (BS.ByteString, BS.ByteString) ), [(BS.ByteString, BS.ByteString)])
rawRequestParser = do
            reqStart <- requestStartParser
            reqHeaders <- rawHeadersParser
            delimeter <- ATP.string (BS.pack "\r\n\r\n")
            return (reqStart, reqHeaders)

requestParser :: ATP.Parser (Request ())
requestParser = do
            (method, (schema, specific), (proto, version) ) <- requestStartParser
            reqHeaders <- headersParser
            delimeter <- ATP.string (BS.pack "\r\n\r\n")
            return (Request (Custom method) (RawURI schema specific) (ProtoVersion proto version) reqHeaders ())


responseStartParser :: ATP.Parser((BS.ByteString, BS.ByteString), Int, BS.ByteString)
responseStartParser = do
               protocol <- token
               ATP.string (BS.pack "/")
               version <- token
               ATP.skipWhile isSpace
               status <- ATPC.decimal
               ATP.skipWhile isSpace
               reason <- ATP.takeWhile1 (\x -> not (ATP.inClass "\r\n" x))
               eol <- ATP.string (crlf)
               return ((protocol, version), status, reason)

rawResponseParser :: ATP.Parser ( ((BS.ByteString, BS.ByteString), Int, BS.ByteString), [(BS.ByteString, BS.ByteString)], ())
rawResponseParser = do
            resStart <- responseStartParser
            resHeaders <- rawHeadersParser
            delimeter <- ATP.string (BS.pack "\r\n\r\n")
            return (resStart, resHeaders, ())

responseParser :: ATP.Parser (Response ())
responseParser = do
            ((proto, version), status, reason) <- responseStartParser
            resHeaders <- headersParser
            delimeter <- ATP.string (BS.pack "\r\n\r\n")
            return (Response (ProtoVersion proto version) status reason resHeaders ())
          

tokenChar :: Word8 -> Bool
tokenChar = ATP.inClass tokenClass

isSpace :: Word8 -> Bool
isSpace = ((fromIntegral (ord ' ')) == )

token :: ATP.Parser BS.ByteString 
token = ATP.takeWhile1 tokenChar

rawViaParser :: ATP.Parser (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString, Int, [(BS.ByteString, BS.ByteString)])
rawViaParser = do
          proto <- token
          ATP.string (BS.pack "/")
          version <- token
          ATP.string (BS.pack "/")
          transport <- token
          ATP.string (BS.pack " ")
          host <- token
          ATP.string (BS.pack ":")
          port <- ATPC.decimal
          paramsP <- paramsParser
          return (proto, version, transport, host, port, paramsP)

viaParser :: ATP.Parser ViaValue
viaParser = do
       (proto, version, transport, host, port, params) <- rawViaParser
       return (ViaValue (ProtoVersion proto version) (Unknown transport) host (fromIntegral port) params)

paramsParser :: ATP.Parser [(BS.ByteString, BS.ByteString)]
paramsParser = option [] (ATP.many1 paramParser)

paramParser :: ATP.Parser (BS.ByteString, BS.ByteString)
paramParser = do
          ATP.string (BS.pack ";")
          name <- token
          ATP.string (BS.pack "=")
          value <- token
          return (name, value)

rawUriParser :: ATP.Parser (BS.ByteString, BS.ByteString)
rawUriParser = do
       schema <- ATP.takeWhile1 (not . (ATP.inClass " :"))
       delim <- ATP.string (BS.pack ":")
       specific <- ATP.takeWhile1 (not . (ATP.inClass " >"))
       return (schema, specific)

uriParser :: ATP.Parser URI
uriParser = do
       schema <- ATP.takeWhile1 (ATP.inClass schemaClass)
       delim <- ATP.string (BS.pack ":")
       if ((schema == (BS.pack "sip")) || (schema == (BS.pack "sips")))
          then sipUriParser schema
          else do
                 specific <- ATP.takeWhile1 (ATP.inClass opaqueUriPartClass)
                 return (RawURI schema specific)

bracketedUriParser :: ATP.Parser URI
bracketedUriParser = do
        ATP.string (BS.pack "<")
        uri <- uriParser
        ATP.string (BS.pack ">")
        return uri

rawSipUriParser :: ATP.Parser (BS.ByteString, BS.ByteString, Maybe Int, [(BS.ByteString, BS.ByteString)])
rawSipUriParser = do
       user <- ATP.takeWhile1 (ATP.inClass userClass)
       delimA <- ATP.string (BS.pack "@")
       host <- ATP.takeWhile1 (ATP.inClass hostClass)
       port <- option Nothing portParser
       paramsP <- paramsParser
       return (user, host, port, paramsP)
   where
       portParser = do
                      delimB <- ATP.string (BS.pack ":")
                      port <- ATPC.decimal
                      return (Just port)


sipUriParser :: BS.ByteString -> ATP.Parser URI
sipUriParser schema = do
       (user, host, port, params) <- rawSipUriParser
       return (SipURI (schema == (BS.pack "sips")) user BS.empty host (port >>= Just . fromIntegral) params)


addressParser :: ATP.Parser SipAddress
addressParser = do
       ATP.skipWhile isSpace
       (displayName, uri, params) <- ((nameAddrParser <|> uriOnlyParser) ATP.<?> "nameAddress or uriOnly")
       return (SipAddress displayName uri params)
    where
       nameAddrParser = do
                          displayName <- (liftM Just quotedNameParser) <|> (liftM Just token) <|> (return Nothing)
                          ATP.skipWhile isSpace
                          uri <- bracketedUriParser
                          params <- paramsParser
                          return (displayName, uri, params)
       uriOnlyParser = do
                          uri <- uriParser
                          return (Nothing, uri, [])

quotedNameParser :: ATP.Parser BS.ByteString
quotedNameParser = do
       ATP.string (BS.pack "\"")
       result <- ATP.takeTill (ATP.inClass "\"\r\n")
       ATP.string (BS.pack "\"")
       return result

messageParser :: ATP.Parser (Either (Response ()) (Request ()))
messageParser = eitherP responseParser requestParser
