package main;

import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;

public interface Query {

    SOAPElement asXml() throws SOAPException;

}