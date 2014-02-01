package main;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.Principal;

import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpException;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpRequest;
import org.apache.http.HttpRequestInterceptor;
import org.apache.http.HttpResponse;
import org.apache.http.auth.AuthScheme;
import org.apache.http.auth.AuthSchemeProvider;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.Credentials;
import org.apache.http.auth.NTCredentials;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.client.config.AuthSchemes;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.entity.HttpEntityWrapper;
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
    
    public static void main(String[] args) throws Exception {
    	Registry<AuthSchemeProvider> authSchemeRegistry = RegistryBuilder.<AuthSchemeProvider>create()
    	        .register(AuthSchemes.NTLM, new JCIFSNTLMSchemeFactory())
    	        .build();
    	
    	CloseableHttpClient httpClient = HttpClients.custom()
    			.setDefaultCredentialsProvider(new CredentialsProvider() {
					
					@Override
					public void setCredentials(AuthScope arg0, Credentials arg1) {
					}
					
					@Override
					public Credentials getCredentials(AuthScope arg0) {
						return new NTCredentials("a554114", "tooBad$3721", "ilag0018", "CORP");
					}
					
					@Override
					public void clear() {
					}
				})
    	        .setDefaultAuthSchemeRegistry(authSchemeRegistry)
    	        .build();
    	HttpPost post = new HttpPost("https://collaborate.statestr.com/sites/CMW/team/_vti_bin/Lists.asmx");
    	post.setEntity(new SOAPMessageEntity(MessageFactory.newInstance().createMessage()));
    	HttpResponse response = httpClient.execute(post);
    	System.out.println(response);
    }
    
    public static class SOAPMessageEntity implements HttpEntity {

    	public SOAPMessageEntity(SOAPMessage msg) {
    		this.msg = msg;
    	}
    	
    	private final SOAPMessage msg;
    	
		@Override
		public void consumeContent() throws IOException {
			throw new UnsupportedOperationException();
		}

		@Override
		public InputStream getContent() throws IOException,
				IllegalStateException {
			throw new UnsupportedOperationException();
		}

		@Override
		public Header getContentEncoding() {
			return null;
		}

		@Override
		public long getContentLength() {
			// TODO Auto-generated method stub
			return -1;
		}

		@Override
		public Header getContentType() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public boolean isChunked() {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public boolean isRepeatable() {
			// TODO Auto-generated method stub
			return true;
		}

		@Override
		public boolean isStreaming() {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public void writeTo(OutputStream arg0) throws IOException {
			try {
				msg.writeTo(arg0);
			} catch (SOAPException e) {
				throw new IOException(e);
			}
		}
    	
    }
    
}