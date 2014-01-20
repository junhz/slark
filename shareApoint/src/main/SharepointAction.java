package main;

import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;

public interface SharepointAction {
    String name();

    SOAPElement[] parameters() throws SOAPException;
}