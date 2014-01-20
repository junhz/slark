package main;

import static main.SharepointNamespace.SP_SOAP;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.LinkedList;
import java.util.List;

import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

public final class CopyIntoItems implements SharepointAction {

    private final URL sourceUrl;
    private final URL[] destinationUrls;
    private final FieldInformation[] fields;

    public CopyIntoItems(Builder builder) {
        this.sourceUrl = builder.sourceUrl;
        if (builder.destinationUrls.isEmpty())
            throw new IllegalStateException("can't copy because no destination is given.");
        this.destinationUrls = builder.destinationUrls.toArray(new URL[builder.destinationUrls.size()]);
        this.fields = builder.fields.toArray(new FieldInformation[builder.fields.size()]);
    }

    @Override
    public String name() {
        return "CopyIntoItems";
    }

    @Override
    public SOAPElement[] parameters() throws SOAPException {

        SOAPElement sourceUrlNode = SP_SOAP.element("SourceUrl");
        sourceUrlNode.addTextNode(sourceUrl.toString());

        SOAPElement destinationUrlsNode = SP_SOAP.element("DestinationUrls");
        for (URL destinationUrl : destinationUrls) {
            destinationUrlsNode.addChildElement(SP_SOAP.qname("string")).addTextNode(destinationUrl.toString());
        }

        SOAPElement fieldsNode = SP_SOAP.element("Fields");
        for (FieldInformation field : fields) {
            fieldsNode.addChildElement(field.asXml());
        }

        SOAPElement stream = SP_SOAP.element("Stream");
        try {
            stream.addTextNode(encodeStream(sourceUrl));
        } catch (IOException e) {
            throw new SOAPException("not a valid source url", e);
        }

        return new SOAPElement[] { sourceUrlNode, destinationUrlsNode, fieldsNode, stream };
    }

    public static Builder builder(URL sourceUrl) {
        return new Builder(sourceUrl);
    }
    
    private static String encodeStream(URL source) throws IOException {
        InputStream in = source.openStream();
        byte[] buffer = new byte[1024];
        int size;
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        while((size = in.read(buffer)) != -1) {
            out.write(buffer, 0, size);
        }
        in.close();
        out.close();
        return javax.xml.bind.DatatypeConverter.printBase64Binary(out.toByteArray());
    }

    private static final class FieldInformation {
        private final String displayName;
        private final String valueType;
        private final String valueText;

        public FieldInformation(String displayName, String valueType, String valueText) {
            this.displayName = displayName;
            this.valueType = valueType;
            this.valueText = valueText;
        }

        public SOAPElement asXml() throws SOAPException {
            SOAPFactory factory = SOAPFactory.newInstance();

            SOAPElement info = factory.createElement(SP_SOAP.qname("FieldInformation"));
            info.setAttribute("DisplayName", displayName);
            info.setAttribute("Type", valueType);
            info.setAttribute("Value", valueText);

            return info;
        }
    }

    public final static class Builder {
        private final URL sourceUrl;
        private final List<URL> destinationUrls = new LinkedList<URL>();
        private final List<FieldInformation> fields = new LinkedList<FieldInformation>();

        public Builder(URL sourceUrl) {
            this.sourceUrl = sourceUrl;
        }

        public Builder addDestination(URL destinationUrl) {
            this.destinationUrls.add(destinationUrl);
            return this;
        }

        public Builder setField(String displayName, String valueType, String valueText) {
            this.fields.add(new FieldInformation(displayName, valueType, valueText));
            return this;
        }

        public CopyIntoItems build() {
            return new CopyIntoItems(this);
        }
    }
}