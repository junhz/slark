package main;

import javax.xml.namespace.QName;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

public enum SharepointNamespace implements Namespace {
    SP_SOAP() {

        @Override
        public QName qname(String localPart) {
            return new QName(url(), localPart);
        }
        
        @Override
        public String url() {
            return "http://schemas.microsoft.com/sharepoint/soap/";
        }

        @Override
        public SOAPElement element(String localPart) throws SOAPException {
            return SOAPFactory.newInstance().createElement(qname(localPart));
        }
        
    }
    
    ;
    
}
