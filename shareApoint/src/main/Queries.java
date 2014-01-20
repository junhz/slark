package main;

import static main.SharepointNamespace.SP_SOAP;

import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFactory;

public enum Queries {
    ;
    public enum Condition {
        EQ("Eq"), CONTAINS("Contains"), GT("Gt");

        private final String name;

        Condition(String name) {
            this.name = name;
        }

        public String conditionName() {
            return name;
        }

        public Query buildQuery(String fieldRefName, String valueType, String valueText) {
            return new SimpleQuery(fieldRefName, valueType, valueText, this);
        }
    }

    private static final class SimpleQuery implements Query {
        private final String fieldRefName;
        private final String valueType;
        private final String valueText;
        private final Condition condition;

        private SimpleQuery(String fieldRefName, String valueType, String valueText, Condition condition) {
            this.fieldRefName = fieldRefName;
            this.valueType = valueType;
            this.valueText = valueText;
            this.condition = condition;
        }

        @Override
        public SOAPElement asXml() throws SOAPException {
            SOAPFactory factory = SOAPFactory.newInstance();

            SOAPElement cond = factory.createElement(SP_SOAP.qname(condition.conditionName()));

            SOAPElement fieldRef = cond.addChildElement(SP_SOAP.qname("FieldRef"));
            fieldRef.setAttribute("Name", fieldRefName);

            SOAPElement value = cond.addChildElement(SP_SOAP.qname("Value"));
            value.setAttribute("Type", valueType);
            value.addTextNode(valueText);

            return cond;
        }

    }

    public static Query and(final Query lhs, final Query rhs) {
        return new Query() {

            @Override
            public SOAPElement asXml() throws SOAPException {
                SOAPFactory factory = SOAPFactory.newInstance();

                SOAPElement e = factory.createElement(SP_SOAP.qname("And"));
                e.addChildElement(lhs.asXml());
                e.addChildElement(rhs.asXml());

                return e;
            }
        };
    }
}