package main;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.xml.soap.SOAPException;

import main.SharepointServer.FileAttributes;

public class SAAJ {

    /**
     * @param args
     * @throws SOAPException 
     * @throws UnsupportedOperationException 
     * @throws IOException 
     * @throws Exception
     */
    public static void main(String[] args) throws SOAPException, IOException {

        URL serverSite = new URL("https://collaborate.statestr.com/sites/CMW/team/");
        URL rootFolder = new URL("https://collaborate.statestr.com/sites/CMW/team/Project Management/Reference data cost breakdown/testUploader");
        String listName = "Project Management";
        File source = new File("C:/Documents and Settings/a554114/workspace/jws/src/main/msg");
        
        String yyyy = new SimpleDateFormat("yyyy").format(new Date());
        
        SharepointServer server = new SharepointServer(serverSite);
        Optional<FileAttributes> rootFolderAttributes = server.find(listName, rootFolder.getPath());
        System.out.println(rootFolderAttributes);
        if (!rootFolderAttributes.hasValue()) {
            throw new IllegalArgumentException("root folder not exists");
        }

        Optional<FileAttributes> dropinAttributes = server.find(listName, rootFolderAttributes.get().getFileRef() + "/" + yyyy);
        System.out.println(dropinAttributes);
        if (!dropinAttributes.hasValue()) {
            if (!server.mkdir(rootFolderAttributes.get(), yyyy)) {
                throw new IllegalArgumentException("can't create folder to dropin file");
            }
        }

        Optional<FileAttributes> destinationAttributes = server.find(listName, rootFolder.getPath() + "/" + yyyy + "/" + source.getName());
        System.out.println(destinationAttributes);
        if (destinationAttributes.hasValue()) {
            if (server.deleteFile(destinationAttributes.get())) {
                throw new IllegalArgumentException("can't delete file " + destinationAttributes.get().getFileRef());
            }
        }

        if (!server.upload(source.toURI().toURL(), new URL(rootFolder.toString() + "/" + yyyy + "/" + source.getName()))) {
            throw new IllegalArgumentException("can't upload file");
        }

        server.stop();
    }

}