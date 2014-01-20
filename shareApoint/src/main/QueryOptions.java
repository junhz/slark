package main;

import static main.SharepointNamespace.SP_SOAP;

import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

public final class QueryOptions {
    private final Optional<String> dateInUtc;
    private final Optional<String> folder;
    // Paging not implemented
    private final Optional<String> includeMandatoryColumns;
    // MeetingInstanceID not implemented
    private final Optional<String> viewAttributes; // only Recursive is used currently so that no enum needed

    private QueryOptions(Builder builder) {
        this.dateInUtc = builder.dateInUtc;
        this.folder = builder.folder;
        this.includeMandatoryColumns = builder.includeMandatoryColumns;
        this.viewAttributes = builder.viewAttributes;
    }

    public SOAPElement asXml() throws SOAPException {
        SOAPFactory factory = SOAPFactory.newInstance();
        SOAPElement queryOptions = factory.createElement(SP_SOAP.qname("QueryOptions"));
        if (dateInUtc.hasValue())
            queryOptions.addChildElement(SP_SOAP.qname("DateInUtc")).addTextNode(dateInUtc.get());
        if (folder.hasValue())
            queryOptions.addChildElement(SP_SOAP.qname("Folder")).addTextNode(folder.get());
        if (includeMandatoryColumns.hasValue())
            queryOptions.addChildElement(SP_SOAP.qname("IncludeMandatoryColumns")).addTextNode(includeMandatoryColumns.get());
        if (viewAttributes.hasValue())
            queryOptions.addChildElement(SP_SOAP.qname("ViewAttributes")).setAttribute("Scope", viewAttributes.get());
        return queryOptions;
    }

    public final static class Builder {
        private Optional<String> dateInUtc = Optional.none();
        private Optional<String> folder = Optional.none();
        private Optional<String> includeMandatoryColumns = Optional.none();
        private Optional<String> viewAttributes = Optional.none();

        private Builder() {
        }

        Builder dateInUtc(boolean b) {
            this.dateInUtc = Optional.some(Boolean.toString(b).toUpperCase());
            return this;
        }

        Builder folder(String s) {
            this.folder = Optional.some(s);
            return this;
        }

        Builder includeMandatoryColumns(boolean b) {
            this.includeMandatoryColumns = Optional.some(Boolean.toString(b).toUpperCase());
            return this;
        }

        Builder viewAttributes(String s) {
            this.viewAttributes = Optional.some(s);
            return this;
        }

        QueryOptions build() {
            return new QueryOptions(this);
        }
    }

    public static Builder builder() {
        return new Builder();
    }

}