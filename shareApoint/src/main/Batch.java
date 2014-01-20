package main;

import java.util.LinkedList;
import java.util.List;

import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;

import static main.SharepointNamespace.*;

public final class Batch {

    private final Optional<String> onError; // Return or Continue
    private final Optional<String> listVersion;
    private final Optional<String> version;
    private final Optional<String> viewName;
    private final Optional<String> rootFolder;
    private final Method[] methods;

    private Batch(Builder builder) {
        this.onError = builder.onError;
        this.listVersion = builder.listVersion;
        this.version = builder.version;
        this.viewName = builder.viewName;
        if (builder.methods.isEmpty())
            throw new IllegalStateException("can't create Batch because no method is given");
        this.rootFolder = builder.rootFolder;
        this.methods = builder.methods.toArray(new Method[builder.methods.size()]);
    }

    public SOAPElement asXml() throws SOAPException {
        SOAPElement batch = SP_SOAP.element("Batch");

        if (onError.hasValue())
            batch.setAttribute("OnError", onError.get());
        if (listVersion.hasValue())
            batch.setAttribute("ListVersion", listVersion.get());
        if (version.hasValue())
            batch.setAttribute("Version", version.get());
        if (viewName.hasValue())
            batch.setAttribute("ViewName", viewName.get());
        if(rootFolder.hasValue()) 
            batch.setAttribute("RootFolder", rootFolder.get());

        for (int i = 0; i < methods.length; i++) {
            SOAPElement methodNode = methods[i].asXml();
            methodNode.setAttribute("ID", Integer.toString(i + 1));
            batch.addChildElement(methodNode);
        }

        return batch;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder {
        private Optional<String> onError = Optional.none(); // Return or Continue
        private Optional<String> listVersion = Optional.none();
        private Optional<String> version = Optional.none();
        private Optional<String> viewName = Optional.none();
        private Optional<String> rootFolder = Optional.none();
        private final List<Method> methods = new LinkedList<Method>();

        private Builder() {
        }

        public Builder onError(String s) {
            this.onError = Optional.some(s);
            return this;
        }

        public Builder listVersion(int i) {
            this.listVersion = Optional.some(Integer.toString(i));
            return this;
        }

        public Builder version(String s) {
            this.version = Optional.some(s);
            return this;
        }

        public Builder viewName(String s) {
            this.viewName = Optional.some(s);
            return this;
        }
        
        public Builder addMethod(Method method) {
            this.methods.add(method);
            return this;
        }
        
        public Builder rootFolder(String s) {
            this.rootFolder = Optional.some(s);
            return this;
        }

        public Batch build() {
            return new Batch(this);
        }

    }

}
