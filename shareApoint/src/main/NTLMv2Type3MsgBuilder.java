package main;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;

public final class NTLMv2Type3MsgBuilder implements Type3MsgBuilder {
	private static final byte[] clientNonce = new byte[] { 0x6a, 0x75, 0x6e, 0x68, 0x7a, 0x20, 0x20, 0x20 };
	private final String encoding;
	private final byte[] nonce;
	private final byte[] targetInfoBytes;

	public NTLMv2Type3MsgBuilder(String encoding, byte[] nonce, byte[] targetInfoBytes) {
		this.encoding = encoding;
		this.nonce = nonce;
		this.targetInfoBytes = targetInfoBytes;
	}

	@Override
	public byte[] build(String host, String dom, String user, String password) throws Exception {
		byte[] ntlmHash = new MD4().digestAll(password.getBytes("UTF-16LE"));
		byte[] ntlmv2Hash = GenType.hmacMD5(concatUserAndDom(user, dom), ntlmHash);
		printBytes(ntlmHash);
		printBytes(ntlmv2Hash);
		byte[] blob = createBlob(System.currentTimeMillis(), clientNonce, targetInfoBytes);
		printBytes(blob);
		byte[] ntlmv2Resp = ByteBuffer.allocate(blob.length + 16)
				.put(GenType.hmacMD5(ByteBuffer.allocate(blob.length + 8).put(nonce).put(blob).array(), ntlmv2Hash))
				.put(blob).array();
		printBytes(GenType.hmacMD5(ByteBuffer.allocate(blob.length + 8).put(nonce).put(blob).array(), ntlmv2Hash));
		byte[] lmv2Resp = ByteBuffer.allocate(24)
				.put(GenType.hmacMD5(ByteBuffer.allocate(16).put(nonce).put(clientNonce).array(), ntlmv2Hash))
				.put(clientNonce).array();
		printBytes(lmv2Resp);
		byte[] domBytes = dom.getBytes(encoding);
		byte[] userBytes = user.getBytes(encoding);
		byte[] hostBytes = host.getBytes(encoding);
		
		int userStart = 64 + domBytes.length;
	    int hostStart = userStart + userBytes.length;
	    int lmRespStart = hostStart + hostBytes.length;
	    int ntRespStart = lmRespStart + lmv2Resp.length;
	    int len = ntRespStart + ntlmv2Resp.length;

	    byte[] protocol = "NTLMSSP\0".getBytes();
	    byte[] type = new byte[] { 0x03, 0x00, 0x00, 0x00 };
	    byte[] lmRespLen = new byte[] { 0x18, 0x00 };
	    byte[] lmRespOff = new byte[] { (byte) lmRespStart, (byte) (lmRespStart >> 8), 0x00, 0x00 };
	    byte[] ntRespLen = new byte[] { 0x18, 0x00 };
	    byte[] ntRespOff = new byte[] { (byte) ntRespStart, (byte) (ntRespStart >> 8), 0x00, 0x00 };
	    byte[] domLen = new byte[] { (byte) domBytes.length, (byte) (domBytes.length >> 8) };
	    byte[] domOff = new byte[] { 0x40, 0x00, 0x00, 0x00 };
	    byte[] userLen = new byte[] { (byte) userBytes.length, (byte) (userBytes.length >> 8) };
	    byte[] userOff = new byte[] { (byte) userStart, (byte) (userStart >> 8), 0x00, 0x00 };
	    byte[] hostLen = new byte[] { (byte) hostBytes.length, (byte) (hostBytes.length >> 8) };
	    byte[] hostOff = new byte[] { (byte) hostStart, (byte) (hostStart >> 8), 0x00, 0x00 };
	    byte[] msgLen = new byte[] { 0x00, 0x00, 0x00, 0x00, (byte) len, (byte) (len >> 8), 0x00, 0x00 };
	    byte[] flags = new byte[] { 0x00, 0x00, 0x00, 0x00 };

	    return ByteBuffer.allocate(len).put(protocol).put(type).put(lmRespLen).put(lmRespLen).put(lmRespOff).put(ntRespLen).put(ntRespLen).put(ntRespOff).put(domLen).put(domLen).put(domOff)
	            .put(userLen).put(userLen).put(userOff).put(hostLen).put(hostLen).put(hostOff).put(msgLen).put(flags).put(domBytes).put(userBytes).put(hostBytes).put(lmv2Resp).put(ntlmv2Resp).array();
	}

	private byte[] concatUserAndDom(String user, String dom) throws UnsupportedEncodingException {
		byte[] userBytes = user.toUpperCase().getBytes("UTF-16LE");
		byte[] domBytes = dom.getBytes("UTF-16LE");
		return ByteBuffer.allocate(userBytes.length + domBytes.length).put(userBytes).put(domBytes).array();
	}

	private byte[] createBlob(Long timeMillis, byte[] clientNonce, byte[] targetInfo) {
		byte[] signature = new byte[] { 0x01, 0x01, 0x00, 0x00 };
		byte[] timeStamp = ByteOrder.LITTLE_ENDIAN.allocate(8).putLong((timeMillis + 11644473600000l) * 10000).array();
		
		return ByteBuffer.allocate(targetInfo.length + 32)
				.put(signature).putInt(0).put(timeStamp).put(clientNonce).putInt(0).put(targetInfo).putInt(0).array();
		
	}
	
	public static void main(String[] args) throws Exception {
		byte[] target = new byte[] { 0x02, 0x00, 0x0c, 0x00, 0x44, 0x00, 0x4f, 0x00,
				  0x4d, 0x00, 0x41, 0x00, 0x49, 0x00, 0x4e, 0x00,
				  0x01, 0x00, 0x0c, 0x00, 0x53, 0x00, 0x45, 0x00, 
				  0x52, 0x00, 0x56, 0x00, 0x45, 0x00, 0x52, 0x00,
				  0x04, 0x00, 0x14, 0x00, 0x64, 0x00, 0x6f, 0x00,
				  0x6d, 0x00, 0x61, 0x00, 0x69, 0x00, 0x6e, 0x00,
				  0x2e, 0x00, 0x63, 0x00, 0x6f, 0x00, 0x6d, 0x00,
				  0x03, 0x00, 0x22, 0x00, 0x73, 0x00, 0x65, 0x00,
				  0x72, 0x00, 0x76, 0x00, 0x65, 0x00, 0x72, 0x00,
				  0x2e, 0x00, 0x64, 0x00, 0x6f, 0x00, 0x6d, 0x00,
				  0x61, 0x00, 0x69, 0x00, 0x6e, 0x00, 0x2e, 0x00,
				  0x63, 0x00, 0x6f, 0x00, 0x6d, 0x00, 0x00, 0x00, 0x00, 0x00 };
		new NTLMv2Type3MsgBuilder("UTF_16LE", new byte[] { 0x01, 0x23, 0x45, 0x67, (byte)0x89, (byte)0xab, (byte)0xcd, (byte)0xef }, target).build("host", "DOMAIN", "user", "SecREt01");
	}
	
	private static void printBytes(byte[] ins) {
        for (int in : ins) {
            System.out.print(String.format("%02x", in & 0xff));
        }
        System.out.println();
    }
	
}