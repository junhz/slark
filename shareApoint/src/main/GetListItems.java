package main;

import static main.SharepointNamespace.SP_SOAP;

import java.util.LinkedList;
import java.util.List;

import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;

public final class GetListItems implements SharepointAction {

    private final String listName;
    private final Optional<String> viewName;
    private final Optional<Query> query;
    // viewField not implemented
    private final Optional<String> rowLimit;
    private final Optional<QueryOptions> queryOptions;
    private final Optional<String> webID;

    public GetListItems(Builder builder) {
        this.listName = builder.listName;
        this.viewName = builder.viewName;
        this.query = builder.query;
        this.rowLimit = builder.rowLimit;
        this.queryOptions = builder.queryOptions;
        this.webID = builder.webID;
    }

    @Override
    public String name() {
        return "GetListItems";
    }

    @Override
    public SOAPElement[] parameters() throws SOAPException {
        List<SOAPElement> params = new LinkedList<SOAPElement>();

        SOAPElement ListNameNode = SP_SOAP.element("listName");
        ListNameNode.addTextNode(listName);
        params.add(ListNameNode);

        if (viewName.hasValue()) {
            SOAPElement viewNameNode = SP_SOAP.element("viewName");
            viewNameNode.addTextNode(viewName.get());
            params.add(viewNameNode);
        }

        if (query.hasValue()) {
            SOAPElement queryNode = SP_SOAP.element("query");
            queryNode.addChildElement(SP_SOAP.qname("Query")).addChildElement(SP_SOAP.qname("Where")).addChildElement(query.get().asXml());
            params.add(queryNode);
        }

        if (rowLimit.hasValue()) {
            SOAPElement rowLimitNode = SP_SOAP.element("rowLimit");
            rowLimitNode.addTextNode(rowLimit.get());
            params.add(rowLimitNode);
        }

        if (queryOptions.hasValue()) {
            SOAPElement queryOptionsNode = SP_SOAP.element("queryOptions");
            queryOptionsNode.addChildElement(queryOptions.get().asXml());
            params.add(queryOptionsNode);
        }

        if (webID.hasValue()) {
            SOAPElement webIDNode = SP_SOAP.element("webID");
            webIDNode.addTextNode(webID.get());
            params.add(webIDNode);
        }

        return params.toArray(new SOAPElement[params.size()]);
    }

    public static Builder builder(String listName) {
        return new Builder(listName);
    }

    public static final class Builder {

        private final String listName;

        private Optional<String> viewName;
        private Optional<Query> query;
        // viewField not implemented
        private Optional<String> rowLimit;
        private Optional<QueryOptions> queryOptions;
        private Optional<String> webID;

        public Builder(String listName) {
            this.listName = listName;
            this.viewName = Optional.none();
            this.query = Optional.none();
            this.rowLimit = Optional.none();
            this.queryOptions = Optional.none();
            this.webID = Optional.none();
        }

        public Builder viewName(String s) {
            this.viewName = Optional.some(s);
            return this;
        }

        public Builder query(Query q) {
            this.query = Optional.some(q);
            return this;
        }

        public Builder rowLimit(int i) {
            this.rowLimit = Optional.some(Integer.toString(i));
            return this;
        }

        public Builder queryOption(QueryOptions q) {
            this.queryOptions = Optional.some(q);
            return this;
        }

        public Builder webID(String s) {
            this.webID = Optional.some(s);
            return this;
        }

        public GetListItems build() {
            return new GetListItems(this);
        }

    }

}