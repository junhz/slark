package main;

import org.apache.http.auth.AuthScheme;
import org.apache.http.auth.AuthSchemeProvider;
import org.apache.http.client.config.AuthSchemes;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.impl.auth.BasicSchemeFactory;
import org.apache.http.impl.auth.DigestSchemeFactory;
import org.apache.http.impl.auth.KerberosSchemeFactory;
import org.apache.http.impl.auth.NTLMScheme;
import org.apache.http.impl.auth.SPNegoSchemeFactory;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.protocol.HttpContext;

public class JCIFSNTLMSchemeFactory implements AuthSchemeProvider {

    public AuthScheme create(final HttpContext context) {
        return new NTLMScheme(new JCIFSEngine());
    }
    
    public static void main(String[] args) {
    	Registry<AuthSchemeProvider> authSchemeRegistry = RegistryBuilder.<AuthSchemeProvider>create()
    	        .register(AuthSchemes.NTLM, new JCIFSNTLMSchemeFactory())
    	        .register(AuthSchemes.BASIC, new BasicSchemeFactory())
    	        .register(AuthSchemes.DIGEST, new DigestSchemeFactory())
    	        .register(AuthSchemes.SPNEGO, new SPNegoSchemeFactory())
    	        .register(AuthSchemes.KERBEROS, new KerberosSchemeFactory())
    	        .build();
    	CloseableHttpClient httpClient = HttpClients.custom()
    	        .setDefaultAuthSchemeRegistry(authSchemeRegistry)
    	        .build();
    }
    
}