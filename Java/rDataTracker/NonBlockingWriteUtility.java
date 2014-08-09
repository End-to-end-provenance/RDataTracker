package rDataTracker;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
 
public class NonBlockingWriteUtility {
    private static String clientChannel = "clientChannel";
    private static String serverChannel = "serverChannel";
    private static String channelType = "channelType";
 
    /**
     * ServerSocketChannel represents a channel for sockets that listen to
     * incoming connections.
     *
     * @throws IOException
     */
    public static void main(String[] args) throws IOException {
        int port = 20140;
        String localhost = "localhost";
 
        // create a new serversocketchannel. The channel is unbound.
        ServerSocketChannel channel = ServerSocketChannel.open();
 
        // bind the channel to an address. The channel starts listening to
        // incoming connections.
        channel.bind(new InetSocketAddress(localhost, port));
 
        // mark the serversocketchannel as non blocking
        channel.configureBlocking(false);
 
        // create a selector that will by used for multiplexing. The selector
        // registers the socketserverchannel as
        // well as all socketchannels that are created
        Selector selector = Selector.open();
 
        // register the serversocketchannel with the selector. The OP_ACCEPT
        // option marks
        // a selection key as ready when the channel accepts a new connection.
        // When the
        // socket server accepts a connection this key is added to the list of
        // selected keys of the selector.
        // when asked for the selected keys, this key is returned and hence we
        // know that a new connection has been accepted.
        SelectionKey socketServerSelectionKey = channel.register(selector,
                SelectionKey.OP_ACCEPT);
        // set property in the key that identifies the channel
        Map<String, String> properties = new HashMap<String, String>();
        properties.put(channelType, serverChannel);
        socketServerSelectionKey.attach(properties);
        // wait for the selected keys
        for (;;) {
 
            // the select method is a blocking method which returns when atleast
            // one of the registered
            // channel is selected. In this example, when the socket accepts a
            // new connection, this method
            // will return. Once a socketclient is added to the list of
            // registered channels, then this method
            // would also return when one of the clients has data to be read or
            // written. It is also possible to perform a nonblocking select
            // using the selectNow() function.
            // We can also specify the maximum time for which a select function
            // can be blocked using the select(long timeout) function.
            if (selector.select() == 0)
                continue;
            // the select method returns with a list of selected keys
            Set<SelectionKey> selectedKeys = selector.selectedKeys();
            Iterator<SelectionKey> iterator = selectedKeys.iterator();
            while (iterator.hasNext()) {
                SelectionKey key = iterator.next();
                // the selection key could either by the socketserver informing
                // that a new connection has been made, or
                // a socket client that is ready for read/write
                // we use the properties object attached to the channel to find
                // out the type of channel.
                if (((Map<String, String>) key.attachment()).get(channelType).equals(
                        serverChannel)) {
                    // a new connection has been obtained. This channel is
                    // therefore a socket server.
                    ServerSocketChannel serverSocketChannel = (ServerSocketChannel) key
                            .channel();
                    // accept the new connection on the server socket. Since the
                    // server socket channel is marked as non blocking
                    // this channel will return null if no client is connected.
                    SocketChannel clientSocketChannel = serverSocketChannel
                            .accept();
 
                    if (clientSocketChannel != null) {
                        // set the client connection to be non blocking
                        clientSocketChannel.configureBlocking(false);
                        SelectionKey clientKey = clientSocketChannel.register(
                                selector, SelectionKey.OP_READ,
                                SelectionKey.OP_WRITE);
                        Map<String, String> clientproperties = new HashMap<String, String>();
                        clientproperties.put(channelType, clientChannel);
                        clientKey.attach(clientproperties);
 
                        // write something to the new created client
                        CharBuffer buffer = CharBuffer.wrap("Hello client");
                        while (buffer.hasRemaining()) {
                            clientSocketChannel.write(Charset.defaultCharset()
                                    .encode(buffer));
                        }
                        buffer.clear();
                    }
 
                } else {
                    // data is available for read
                    // buffer for reading
                    ByteBuffer buffer = ByteBuffer.allocate(20);
                    SocketChannel clientChannel = (SocketChannel) key.channel();
                    int bytesRead = 0;
                    if (key.isReadable()) {
                        // the channel is non blocking so keep it open till the
                        // count is >=0
                        if ((bytesRead = clientChannel.read(buffer)) > 0) {
                            buffer.flip();
//                            System.out.println(Charset.defaultCharset().decode(
//                                    buffer));
                            buffer.clear();
                        }
                        if (bytesRead < 0) {
                            // the key is automatically invalidated once the
                            // channel is closed
                            clientChannel.close();
                        }
                    }
 
                }
 
                // once a key is handled, it needs to be removed
                iterator.remove();
 
            }
        }
 
    }
}