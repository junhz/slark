package main;

import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPHeader;
import javax.xml.soap.SOAPMessage;

import static main.SharepointNamespace.*;

public final class SharepointRequest {

    private final SOAPMessage request;

    private SharepointRequest() throws SOAPException {
        request = MessageFactory.newInstance().createMessage();
    }

    public static SharepointRequest newInstance(SharepointAction action) throws SOAPException {
        SharepointRequest builder = new SharepointRequest();
        SOAPEnvelope envelope = builder.request.getSOAPPart().getEnvelope();
        SOAPHeader header = envelope.getHeader();
        SOAPBody body = envelope.getBody();

        SOAPBodyElement actionElement = body.addBodyElement(SP_SOAP.qname(action.name()));
        SOAPElement[] parameters = action.parameters();
        for (SOAPElement param : parameters) {
            actionElement.addChildElement(param);
        }

        builder.request.getMimeHeaders().setHeader("SOAPAction", SP_SOAP.url() + action.name());

        return builder;
    }

    public SOAPMessage build() {
        return request;
    }

}
