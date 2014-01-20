package main;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.util.Arrays;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.SecretKeySpec;
import javax.xml.bind.DatatypeConverter;

/**
 * http://www.innovation.ch/personal/ronald/ntlm.html
 * http://alvinalexander.com/java/jwarehouse/commons-httpclient-2.0.1/src/java/org/apache/commons/httpclient/NTLM.shtml
 * 
 * @author a554114
 * 
 */
public class GenType {

    /**
     * @param args
     * @throws Exception 
     */
    public static void main(String[] args) throws Exception {
        String user = prompt("user: ");
        String host = prompt("host: ");
        String dom = prompt("domain: ");
        String password = prompt("password: ");
        
        System.out.println(DatatypeConverter.printBase64Binary(encodeType1Message(host.toUpperCase().getBytes(), dom.toUpperCase().getBytes(), NTLMFlags.NEGOTIATE_UNICODE)));
        String type2Message = prompt("replied type2message: ");
        printBytes(DatatypeConverter.parseBase64Binary(type2Message));
        Type3MsgBuilder builder = decodeType2Message(DatatypeConverter.parseBase64Binary(type2Message));
        System.out.println(DatatypeConverter.printBase64Binary(builder.build(host, dom, user, password)));
    }
    
    private static String prompt(String msg) throws IOException {
        System.out.println(msg);
        return new BufferedReader(new InputStreamReader(System.in)).readLine();
    }

    private static void printBytes(byte[] ins) {
        for (int in : ins) {
            System.out.print(String.format("%02X ", in & 0xff));
        }
        System.out.println();
    }

    private static byte[] encodeType1Message(byte[] host, byte[] dom, NTLMFlags... flags) {
        byte[] protocol = "NTLMSSP\0".getBytes();
        byte[] type = new byte[] { 0x01, 0x00, 0x00, 0x00 };
        byte[] domLen = new byte[] { (byte) dom.length, (byte) (dom.length >> 8) };
        byte[] domOff = new byte[] { (byte) (32 + host.length), (byte) ((32 + host.length) >> 8), 0x00, 0x00 };
        byte[] hostLen = new byte[] { (byte) (host.length), (byte) (host.length >> 8) };
        byte[] hostOff = new byte[] { 0x20, 0x00, 0x00, 0x00 };

        return ByteBuffer.allocate(32 + host.length + dom.length).put(protocol).put(type).put(ByteOrder.LITTLE_ENDIAN.allocate(4).putInt(NTLMFlags.intValue(flags)).array()).put(domLen).put(domLen).put(domOff).put(hostLen).put(hostLen).put(hostOff).put(host).put(dom)
                .array();
    }

    private static Type3MsgBuilder decodeType2Message(byte[] msg) {
    	int flags = ByteOrder.LITTLE_ENDIAN.wrap(Arrays.copyOfRange(msg, 20, 24)).getInt();
    	final byte[] nonce = Arrays.copyOfRange(msg, 24, 32);
        final String encoding;
    	if(NTLMFlags.supportUnicode(flags)) {
    		encoding = "UTF-16LE";
    	} else {
    		encoding = "US-ASCII";
    	}
    	
    	if(NTLMFlags.echoTargetInfo(flags)) {
    		short targetInfoLen = ByteOrder.LITTLE_ENDIAN.wrap(Arrays.copyOfRange(msg, 40, 42)).getShort();
    		int targetInfoOff = ByteOrder.LITTLE_ENDIAN.wrap(Arrays.copyOfRange(msg, 44, 48)).getInt();
    		final byte[] targetInfoBytes = Arrays.copyOfRange(msg, targetInfoOff, targetInfoOff + targetInfoLen);
    		
    		return new NTLMv2Type3MsgBuilder(encoding, nonce, targetInfoBytes);
    	} else {
    		throw new UnsupportedOperationException();
    	}
    }

    private static byte[] encodeType3Message(byte[] host, byte[] dom, byte[] user, String password, byte[] nonce) throws InvalidKeyException, NoSuchAlgorithmException, NoSuchPaddingException,
            IllegalBlockSizeException, BadPaddingException, InvalidKeySpecException, UnsupportedEncodingException {
        byte[] lmRes = calcResponse(calclmHash(password), nonce);
        byte[] ntRes = calcResponse(calcNtHash(password), nonce);
        System.out.println(DatatypeConverter.printBase64Binary(lmRes));
        System.out.println(DatatypeConverter.printBase64Binary(ntRes));
        int userStart = 64 + dom.length;
        int hostStart = userStart + user.length;
        int lmRespStart = hostStart + host.length;
        int ntRespStart = lmRespStart + 24;
        int len = ntRespStart + 24;

        byte[] protocol = "NTLMSSP\0".getBytes();
        byte[] type = new byte[] { 0x03, 0x00, 0x00, 0x00 };
        byte[] lmRespLen = new byte[] { 0x18, 0x00 };
        byte[] lmRespOff = new byte[] { (byte) lmRespStart, (byte) (lmRespStart >> 8), 0x00, 0x00 };
        byte[] ntRespLen = new byte[] { 0x18, 0x00 };
        byte[] ntRespOff = new byte[] { (byte) ntRespStart, (byte) (ntRespStart >> 8), 0x00, 0x00 };
        byte[] domLen = new byte[] { (byte) dom.length, (byte) (dom.length >> 8) };
        byte[] domOff = new byte[] { 0x40, 0x00, 0x00, 0x00 };
        byte[] userLen = new byte[] { (byte) user.length, (byte) (user.length >> 8) };
        byte[] userOff = new byte[] { (byte) userStart, (byte) (userStart >> 8), 0x00, 0x00 };
        byte[] hostLen = new byte[] { (byte) host.length, (byte) (host.length >> 8) };
        byte[] hostOff = new byte[] { (byte) hostStart, (byte) (hostStart >> 8), 0x00, 0x00 };
        byte[] msgLen = new byte[] { 0x00, 0x00, 0x00, 0x00, (byte) len, (byte) (len >> 8), 0x00, 0x00 };
        byte[] flags = new byte[] { 0x01, (byte) 0x82, 0x00, 0x00 };

        return ByteBuffer.allocate(len).put(protocol).put(type).put(lmRespLen).put(lmRespLen).put(lmRespOff).put(ntRespLen).put(ntRespLen).put(ntRespOff).put(domLen).put(domLen).put(domOff)
                .put(userLen).put(userLen).put(userOff).put(hostLen).put(hostLen).put(hostOff).put(msgLen).put(flags).put(dom).put(user).put(host).put(lmRes).put(ntRes).array();
    }
    
    private static byte[] encodeType3Messagev2(byte[] host, byte[] dom, byte[] user, String password, byte[] nonce) throws NoSuchAlgorithmException, InvalidKeyException, NoSuchPaddingException, IllegalBlockSizeException, BadPaddingException, InvalidKeySpecException, UnsupportedEncodingException {
        byte[] clientNonce = new byte[] { (byte)0xff, (byte)0xff, (byte)0xff, 0x00, 0x11, 0x22, 0x33, 0x44 };
        byte[] lmRes = ByteBuffer.allocate(24).put(clientNonce).array();
        byte[] sessionNonce = ByteBuffer.allocate(16).put(nonce).put(clientNonce).array();
        byte[] sessionHash = Arrays.copyOfRange(MessageDigest.getInstance("MD5").digest(sessionNonce), 0, 8);
        byte[] ntRes = calcResponse(calcNtHash(password), sessionHash);
        
        int userStart = 64 + dom.length;
        int hostStart = userStart + user.length;
        int lmRespStart = hostStart + host.length;
        int ntRespStart = lmRespStart + 24;
        int len = ntRespStart + 24;
        
        byte[] protocol = "NTLMSSP\0".getBytes();
        byte[] type = new byte[] { 0x03, 0x00, 0x00, 0x00 };
        byte[] lmRespLen = new byte[] { 0x18, 0x00 };
        byte[] lmRespOff = new byte[] { (byte) lmRespStart, (byte) (lmRespStart >> 8), 0x00, 0x00 };
        byte[] ntRespLen = new byte[] { 0x18, 0x00 };
        byte[] ntRespOff = new byte[] { (byte) ntRespStart, (byte) (ntRespStart >> 8), 0x00, 0x00 };
        byte[] domLen = new byte[] { (byte) dom.length, (byte) (dom.length >> 8) };
        byte[] domOff = new byte[] { 0x40, 0x00, 0x00, 0x00 };
        byte[] userLen = new byte[] { (byte) user.length, (byte) (user.length >> 8) };
        byte[] userOff = new byte[] { (byte) userStart, (byte) (userStart >> 8), 0x00, 0x00 };
        byte[] hostLen = new byte[] { (byte) host.length, (byte) (host.length >> 8) };
        byte[] hostOff = new byte[] { (byte) hostStart, (byte) (hostStart >> 8), 0x00, 0x00 };
        byte[] msgLen = new byte[] { 0x00, 0x00, 0x00, 0x00, (byte) len, (byte) (len >> 8), 0x00, 0x00 };
        byte[] flags = new byte[] { 0x01, (byte) 0x82, 0x00, 0x00 };

        return ByteBuffer.allocate(len).put(protocol).put(type).put(lmRespLen).put(lmRespLen).put(lmRespOff).put(ntRespLen).put(ntRespLen).put(ntRespOff).put(domLen).put(domLen).put(domOff)
                .put(userLen).put(userLen).put(userOff).put(hostLen).put(hostLen).put(hostOff).put(msgLen).put(flags).put(dom).put(user).put(host).put(lmRes).put(ntRes).array();
    }

    private static byte[] calclmHash(String password) throws NoSuchAlgorithmException, NoSuchPaddingException, InvalidKeyException, InvalidKeySpecException, IllegalBlockSizeException,
            BadPaddingException, UnsupportedEncodingException {
        byte[] asciiPassword = password.toUpperCase().getBytes("US-ASCII");
        byte[][] keys = new byte[2][7];
        for (int i = 0; i < 14 && i < asciiPassword.length; i++) {
            keys[i / 7][i % 7] = asciiPassword[i];
        }
        byte[] magic = new byte[] { 0x4B, 0x47, 0x53, 0x21, 0x40, 0x23, 0x24, 0x25 };
        return ByteBuffer.allocate(16).put(encrypt(desKey(keys[0]), magic)).put(encrypt(desKey(keys[1]), magic)).array();
    }

    private static byte[] encrypt(byte[] key, byte[] text) throws NoSuchAlgorithmException, NoSuchPaddingException, InvalidKeyException, IllegalBlockSizeException, BadPaddingException,
            InvalidKeySpecException {
        Cipher ecipher = Cipher.getInstance("DES/ECB/NoPadding");
        ecipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "DES"));
        return ecipher.doFinal(text);
    }

    /**
     * Creates the NTLM Hash of the user's password.
     * 
     * @param password
     *            The password.
     * 
     * @return The NTLM Hash of the given password, used in the calculation
     *         of the NTLM Response and the NTLMv2 and LMv2 Hashes.
     * @throws UnsupportedEncodingException
     * @throws NoSuchAlgorithmException
     */
    private static byte[] calcNtHash(String password) throws UnsupportedEncodingException, NoSuchAlgorithmException {
        byte[] unicodePassword = password.getBytes("UTF-16LE");
        return new MD4().digestAll(unicodePassword);
    }

    private static byte[] calcResponse(byte[] key16, byte[] challenge) throws InvalidKeyException, NoSuchAlgorithmException, NoSuchPaddingException, IllegalBlockSizeException, BadPaddingException,
            InvalidKeySpecException {
        printBytes(key16);
        byte[] padded = ByteBuffer.allocate(21).put(key16).array();
        byte[][] newKeys = new byte[3][7];
        for (int i = 0; i < 21; i++) {
            newKeys[i / 7][i % 7] = padded[i];
        }
        return ByteBuffer.allocate(24).put(encrypt(desKey(newKeys[0]), challenge)).put(encrypt(desKey(newKeys[1]), challenge)).put(encrypt(desKey(newKeys[2]), challenge)).array();
    }

    private static byte[] desKey(byte[] key56) {
        byte[] key = new byte[8];
        key[0] = oddParity(key56[0]);
        key[1] = oddParity((byte) ((key56[0] << 7) | ((key56[1] & 0xff) >> 1)));
        key[2] = oddParity((byte) ((key56[1] << 6) | ((key56[2] & 0xff) >> 2)));
        key[3] = oddParity((byte) ((key56[2] << 5) | ((key56[3] & 0xff) >> 3)));
        key[4] = oddParity((byte) ((key56[3] << 4) | ((key56[4] & 0xff) >> 4)));
        key[5] = oddParity((byte) ((key56[4] << 3) | ((key56[5] & 0xff) >> 5)));
        key[6] = oddParity((byte) ((key56[5] << 2) | ((key56[6] & 0xff) >> 6)));
        key[7] = oddParity((byte) (key56[6] << 1));

        return key;
    }
    
    /**
     * Calculates the HMAC-MD5 hash of the given data using the specified
     * hashing key.
     *
     * @param data The data for which the hash will be calculated. 
     * @param key The hashing key.
     *
     * @return The HMAC-MD5 hash of the given data.
     */
    static byte[] hmacMD5(byte[] data, byte[] key) throws Exception {
        byte[] ipad = new byte[64];
        byte[] opad = new byte[64];
        for (int i = 0; i < 64; i++) {
            ipad[i] = (byte) 0x36;
            opad[i] = (byte) 0x5c;
        }
        for (int i = key.length - 1; i >= 0; i--) {
            ipad[i] ^= key[i];
            opad[i] ^= key[i];
        }
        byte[] content = new byte[data.length + 64];
        System.arraycopy(ipad, 0, content, 0, 64);
        System.arraycopy(data, 0, content, 64, data.length);
        MessageDigest md5 = MessageDigest.getInstance("MD5");
        data = md5.digest(content);
        content = new byte[data.length + 64];
        System.arraycopy(opad, 0, content, 0, 64);
        System.arraycopy(data, 0, content, 64, data.length);
        return md5.digest(content);
    }

    /**
     * Applies odd parity to the given byte array.
     * 
     * @param bytes
     *            The data whose parity bits are to be adjusted for
     *            odd parity.
     */
    private static byte oddParity(byte b) {
        boolean needsParity = (((b >>> 7) ^ (b >>> 6) ^ (b >>> 5) ^ (b >>> 4) ^ (b >>> 3) ^ (b >>> 2) ^ (b >>> 1)) & 0x01) == 0;
        if (needsParity) {
            return (byte) (b | 0x01);
        } else {
            return (byte) (b & 0xfe);
        }
    }
    
}
