> import           System.Environment    (getArgs)
> import           Control.Concurrent    (threadDelay, forkIO)
> import           Control.Monad         (void)
> import           System.IO             (IOMode (ReadWriteMode))

> import           Network               (withSocketsDo)
> import qualified Network.Simple.TCP    as NST
> import           Network.Socket        (socketToHandle)
> import qualified Network.Socket.Splice as Splice

> import qualified Network.Anonymous.I2P.Types.Socket  as S
> import qualified Network.Anonymous.I2P.Types.Session as S
> import qualified Network.Anonymous.I2P               as I2P

Our main function is fairly simple: get our configuration data and start
a new I2P Session. As soon as this session completes, exit the program.

> main = withSocketsDo $ do
>   port <- getPort

Once we got the configuration data, launch a Tor session and will continue
execution in the 'withinSession' function.

>   I2P.withSession I2P.defaultEndPoint S.VirtualStream (withinSession port)
>
>   where

Some boilerplate code: we need to set up a port mapping, and rather than hard-
coding it, we allow the user to provide it as command line arguments. We will
accept a single argument, which is the private service we want to redirect
connections to (I2P doesn't support ports, only endpoints).

>     getPort :: IO Integer
>     getPort =
>       fmap (read . head) getArgs

Once an I2P session has been created and we are connected with the I2P SAM
bridge, let's set up a new endpoint which redirects incoming connections to the
'newConnection' function.

>     withinSession port ctx = do
>       I2P.serveStream ctx (newConnection port)

If we would leave this function at this point, our connection with the Tor
control service would be lost, which would cause Tor to clean up any mappings
and hidden services we have registered.

Since this is just an example, we will now wait for 5 minutes and then exit.

>       threadDelay 300000000

This function is called for all incoming connections. All it needs to do is
establish a connection with the local service and relay the connections to the
local, private server.

>     newConnection port (sPublic, _) =
>       NST.connect "127.0.0.1" (show port) $ \(sPrivate, addr) -> spliceSockets sPublic sPrivate

And to demonstrate that the sockets we deal with are just regular, normal
network sockets, we implement a function using the `splice` package that
creates a bidirectional pipe between the public and private sockets.

>     spliceSockets sLhs sRhs = do
>       hLhs <- socketToHandle sLhs ReadWriteMode
>       hRhs <- socketToHandle sRhs ReadWriteMode
>       _ <- forkIO $ Splice.splice 1024 (sLhs, Just hLhs) (sRhs, Just hRhs)
>       Splice.splice 1024 (sRhs, Just hRhs) (sLhs, Just hLhs)
