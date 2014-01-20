package main;

import javax.xml.namespace.QName;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;

public interface Namespace {
    
    QName qname(String localPart);
    
    SOAPElement element(String localPart) throws SOAPException;
    
    String url();
    
}
