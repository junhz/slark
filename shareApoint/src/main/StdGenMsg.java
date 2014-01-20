package main;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import jcifs.ntlmssp.NtlmFlags;
import jcifs.ntlmssp.Type1Message;
import jcifs.ntlmssp.Type2Message;
import jcifs.ntlmssp.Type3Message;
import jcifs.util.Base64;

public class StdGenMsg {

	public static void main(String[] args) throws IOException {
		String user = prompt("user: ");
        String host = prompt("host: ");
        String dom = prompt("domain: ");
        String password = prompt("password: ");
        
        System.out.println(generateType1Msg(dom, host));
        
        String type2Message = prompt("replied type2message: ");
        
        System.out.println(generateType3Msg(user, password, dom, host, type2Message));
	}
	
	private static String prompt(String msg) throws IOException {
        System.out.println(msg);
        return new BufferedReader(new InputStreamReader(System.in)).readLine();
    }

	private static final int TYPE_1_FLAGS = 
            NtlmFlags.NTLMSSP_NEGOTIATE_56 | 
            NtlmFlags.NTLMSSP_NEGOTIATE_128 | 
            NtlmFlags.NTLMSSP_NEGOTIATE_NTLM2 | 
            NtlmFlags.NTLMSSP_NEGOTIATE_ALWAYS_SIGN | 
            NtlmFlags.NTLMSSP_REQUEST_TARGET;

    public static String generateType1Msg(final String domain, final String workstation) {
        final Type1Message type1Message = new Type1Message(TYPE_1_FLAGS, domain, workstation);
        return Base64.encode(type1Message.toByteArray());
    }

    public static String generateType3Msg(final String username, final String password,
            final String domain, final String workstation, final String challenge) throws IOException {
        Type2Message type2Message;
        type2Message = new Type2Message(Base64.decode(challenge));
        final int type2Flags = type2Message.getFlags();
        final int type3Flags = type2Flags
                & (0xffffffff ^ (NtlmFlags.NTLMSSP_TARGET_TYPE_DOMAIN | NtlmFlags.NTLMSSP_TARGET_TYPE_SERVER));
        final Type3Message type3Message = new Type3Message(type2Message, password, domain,
                username, workstation, type3Flags);
        return Base64.encode(type3Message.toByteArray());
    }
	
}
