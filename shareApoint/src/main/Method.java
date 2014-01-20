package main;

import java.util.LinkedList;
import java.util.List;

import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;

import static main.SharepointNamespace.*;

public final class Method {

    private final String cmd;
    private final Field[] fields;

    public Method(Builder builder) {
        this.cmd = builder.cmd;
        if (builder.fields.isEmpty())
            throw new IllegalStateException("can't create method because no field is given");
        this.fields = builder.fields.toArray(new Field[builder.fields.size()]);
    }

    public SOAPElement asXml() throws SOAPException {
        SOAPElement method = SP_SOAP.element("Method");
        method.setAttribute("Cmd", cmd);
        for (Field field : fields) {
            method.addChildElement(field.asXml());
        }
        return method;
    }

    public static Builder builder(String cmd) {
        return new Builder(cmd);
    }

    private final static class Field {
        private final String name;
        private final String value;

        public Field(String name, String value) {
            this.name = name;
            this.value = value;
        }

        public SOAPElement asXml() throws SOAPException {
            SOAPElement field = SP_SOAP.element("Field");
            field.setAttribute("Name", name);
            field.addTextNode(value);
            return field;
        }

    }

    public final static class Builder {

        private final String cmd;
        private final List<Field> fields = new LinkedList<Method.Field>();

        private Builder(String cmd) {
            this.cmd = cmd;
        }

        public Builder addField(String name, String value) {
            this.fields.add(new Field(name, value));
            return this;
        }

        public Method build() {
            return new Method(this);
        }

    }

}
