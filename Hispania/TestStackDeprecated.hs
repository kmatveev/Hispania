module Hispania.TestStackDeprecated where




inject :: BS.ByteString -> TestConfig -> Stack -> IO Stack
inject msg testconf stack = handle msg stack (incomingDirection testconf) (testTransport testconf)


incomingRegisterStr = "REGISTER sip:user@host SIP/2.0\r\nFrom: User <sip:user@domain.dom>\r\nTo: User <sip:user@domain.dom>\r\nCall-ID: 123321\r\nVia: SIP/2.0/UDP 127.0.0.1:5060;branch=123123123123\r\n\r\n"
incomingRegister = BS.pack incomingRegisterStr

testIncomingRegister = do
                         initializedStack <- initStack (newStack printingHandler)
                         stackAfterReq <- inject incomingRegister defaultConfig initializedStack
                         return stackAfterReq

incomingDirection :: TestConfig -> Direction
incomingDirection testconf = Direction (localAddr , (Just remoteAddr))
  where
     localAddr = SockAddrInet (testLocalPort testconf) iNADDR_ANY
     remoteAddr = SockAddrInet (testRemotePort testconf) iNADDR_ANY

testServer = start (newStack simpleResponder)

start :: Stack -> IO ()
start stack  = do
                 initilaizedStack <- initStack stack
                 serveLoop initilaizedStack
