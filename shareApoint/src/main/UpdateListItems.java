package main;

import static main.SharepointNamespace.SP_SOAP;

import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;

public final class UpdateListItems implements SharepointAction {

    private final String listName;
    private final Batch batch;

    @Override
    public String name() {
        return "UpdateListItems";
    }

    public UpdateListItems(String listName, Batch batch) {
        this.listName = listName;
        this.batch = batch;
    }

    @Override
    public SOAPElement[] parameters() throws SOAPException {

        SOAPElement listNameNode = SP_SOAP.element("listName");
        listNameNode.addTextNode(listName);

        SOAPElement updates = SP_SOAP.element("updates");
        updates.addChildElement(batch.asXml());

        return new SOAPElement[] { listNameNode, updates };
    }

}