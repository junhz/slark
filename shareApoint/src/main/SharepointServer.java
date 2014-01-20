package main;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Map;

import javax.net.ssl.HttpsURLConnection;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPConnection;
import javax.xml.soap.SOAPConnectionFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

public class SharepointServer {

    private final URL listsAddress;
    private final URL copyAddress;
    private final SOAPConnection connection;

    public SharepointServer(URL site) throws MalformedURLException, UnsupportedOperationException, SOAPException {
        listsAddress = new URL(site, "./_vti_bin/Lists.asmx");
        copyAddress = new URL(site, "./_vti_bin/copy.asmx");
        connection = SOAPConnectionFactory.newInstance().createConnection();
    }

    public Optional<FileAttributes> find(String listName, String relativePart) throws UnsupportedOperationException, SOAPException, IOException {
        Query fileRefEq = Queries.Condition.EQ.buildQuery("FileRef", "Text", relativePart);
        QueryOptions rec = QueryOptions.builder().viewAttributes("RecursiveAll").build();

        SOAPMessage request = SharepointRequest.newInstance(GetListItems.builder(listName).query(fileRefEq).queryOption(rec).build()).build();
        request.writeTo(System.out);
        System.out.println();

        SOAPMessage repsonse = connection.call(request, listsAddress);
        repsonse.writeTo(System.out);
        System.out.println();

        NodeList rows = repsonse.getSOAPBody().getElementsByTagNameNS("#RowsetSchema", "row");
        if (rows.getLength() != 1)
            return Optional.none();
        else
            return Optional.some(new FileAttributes(listName, rows.item(0).getAttributes()));
    }

    public boolean upload(URL source, URL destination) throws UnsupportedOperationException, SOAPException, IOException {
        SOAPMessage request = SharepointRequest.newInstance(CopyIntoItems.builder(source).addDestination(destination).build())
                .build();
        request.writeTo(System.out);
        System.out.println();

        SOAPMessage response = connection.call(request, copyAddress);
        response.writeTo(System.out);
        System.out.println();

        NodeList rows = response.getSOAPBody().getElementsByTagName("CopyResult");
        if (rows.getLength() != 1)
            return false;
        String errorCode = rows.item(0).getAttributes().getNamedItem("ErrorCode").getNodeValue();
        if (errorCode.equals("Success"))
            return true;
        else
            return false;
    }

    public boolean deleteFile(FileAttributes fileAttributes) throws UnsupportedOperationException, SOAPException, IOException {
        Method delete = Method.builder("Delete").addField("ID", Integer.toString(fileAttributes.getId())).addField("FileRef", fileAttributes.getFileRef()).build();
        Batch batch = Batch.builder().onError("Continue").addMethod(delete).build();

        SOAPMessage request = SharepointRequest.newInstance(new UpdateListItems(fileAttributes.getListName(), batch)).build();
        request.writeTo(System.out);
        System.out.println();

        SOAPMessage response = connection.call(request, listsAddress);
        response.writeTo(System.out);
        System.out.println();

        NodeList rows = response.getSOAPBody().getElementsByTagName("Result");
        if (rows.getLength() != 1)
            return false;
        if ("0x00000000".equals(rows.item(0).getChildNodes().item(0).getTextContent()))
            return true;
        else
            return false;
    }

    public boolean mkdir(FileAttributes parent, String name) throws SOAPException, IOException {
        Method createFolder = Method.builder("New").addField("ID", "New").addField("FSObjType", "1").addField("BaseName", name).build();
        Batch batch = Batch.builder().onError("Continue").rootFolder("/" + parent.getFileRef()).addMethod(createFolder).build();

        SOAPMessage request = SharepointRequest.newInstance(new UpdateListItems(parent.getListName(), batch)).build();
        request.writeTo(System.out);
        System.out.println();

        SOAPMessage response = connection.call(request, listsAddress);
        response.writeTo(System.out);
        System.out.println();

        NodeList rows = response.getSOAPBody().getElementsByTagName("Result");
        if (rows.getLength() != 1)
            return false;
        if ("0x00000000".equals(rows.item(0).getChildNodes().item(0).getTextContent()))
            return true;
        else
            return false;
    }

    final class FileAttributes {
        private final String listName;
        private final int id;
        private final String fileRef;
        private final int fsObjType;

        public FileAttributes(String listName, NamedNodeMap attribtues) {
            this.listName = listName;
            this.id = Integer.parseInt(attribtues.getNamedItem("ows_ID").getNodeValue());
            this.fileRef = attribtues.getNamedItem("ows_FileRef").getNodeValue().split(";#")[1];
            this.fsObjType = Integer.parseInt(attribtues.getNamedItem("ows_FSObjType").getNodeValue().split(";#")[1]);
        }

        public String getListName() {
            return listName;
        }

        public int getId() {
            return id;
        }

        public String getFileRef() {
            return fileRef;
        }

        public int getFsObjType() {
            return fsObjType;
        }

        @Override
        public String toString() {
            return "listName = " + listName + ", ID = " + id + ", FileRef = " + fileRef + ", FSObjType = " + fsObjType;
        }

    }
    
    public void stop() throws SOAPException {
        connection.close();
    }

    /**
     * http://stackoverflow.com/questions/2793150/how-to-use-java-net-urlconnection-to-fire-and-handle-http-requests
     * http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/7-b147/com/sun/xml/internal/messaging/saaj/client/p2p/HttpSOAPConnection.java#HttpSOAPConnection.d%28java.lang.String%29
     * @param args
     * @throws UnsupportedOperationException
     * @throws SOAPException
     * @throws IOException
     */
    public static void main(final String[] args) throws UnsupportedOperationException, SOAPException, IOException {
        /*final ServerSocketChannel channel = ServerSocketChannel.open();
        channel.socket().bind(new InetSocketAddress(10086));
        new Thread(new Runnable() {

            @Override
            public void run() {
                try {
                    int size;
                    SocketChannel c = channel.accept();
                    ByteBuffer buffer = ByteBuffer.allocate(1024);
                    while ((size = c.read(buffer)) != -1) {
                        System.out.print(new String(buffer.array(), 0, size));
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }).start();
        SOAPMessage request = MessageFactory.newInstance().createMessage();
        SOAPHeaderElement auth = request.getSOAPHeader().addHeaderElement(new QName("http://soap-authentication.org/basic/2001/10/", "BasicAuth", "auth"));
        auth.addChildElement(new QName("Name")).addTextNode("CORP\\a554114");
        auth.addChildElement(new QName("Password")).addTextNode("tooBad$3721");
        auth.addAttribute(new QName("http://schemas.xmlsoap.org/soap/envelope/", "mustUnderstand", "SOAP-ENV"), "1");
        request.writeTo(System.out);
        System.out.println();
       request.getMimeHeaders().setHeader("Authorization", "Basic "+ javax.xml.bind.DatatypeConverter.printBase64Binary("CORP\\a554114:tooBad$3721".getBytes()));
        SOAPConnectionFactory.newInstance().createConnection().call(request, new URL("http://localhost:10086/sites/CMW/team/_vti_bin/copy.asmx"));*/
        //SOAPConnectionFactory.newInstance().createConnection().call(MessageFactory.newInstance().createMessage(), new URL("https://collaborate.statestr.com/sites/CMW/team/_vti_bin/Lists.asmx")).writeTo(System.out);
        HttpsURLConnection conn;
        //conn.setRequestProperty("SOAPAction", "GetListItems");
        do {
            conn = (HttpsURLConnection) new URL("https://collaborate.statestr.com/sites/CMW/team/_vti_bin/Lists.asmx").openConnection();
            conn.setDoOutput(true);
            conn.setDoInput(true);
            conn.setUseCaches(false);
            conn.setRequestMethod("POST");
            String auth = new BufferedReader(new InputStreamReader(System.in)).readLine();
            conn.setRequestProperty("Authorization", auth);
            conn.connect();
            OutputStream out = conn.getOutputStream();

            MessageFactory.newInstance().createMessage().writeTo(out);

            out.close();
            for (Map.Entry<String, List<String>> entry : conn.getHeaderFields().entrySet()) {
                for (String value : entry.getValue()) {
                    System.out.println(entry.getValue() + ": " + value);
                }
                System.out.println();
            }
            InputStream error = conn.getErrorStream();
            if (error != null) {
                int size;
                byte[] buffer = new byte[1024];
                while ((size = error.read(buffer)) != -1) {
                    System.out.write(buffer, 0, size);
                }
                System.out.println();
                error.close();
            }
        } while (conn.getResponseCode() != 200);
    }

}
