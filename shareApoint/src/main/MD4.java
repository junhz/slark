package main;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.ByteBuffer;

public class MD4 {

    public static void main(String[] args) throws IOException {
        MD4 md4 = new MD4();
        while (true) {
            printBytes(md4.digestAll(new BufferedReader(new InputStreamReader(System.in)).readLine().getBytes("UnicodeLittleUnmarked")));
        }
    }

    private static void printBytes(byte[] ins) {
        for(int in : ins) {
            System.out.print(Integer.toHexString(in) + " ");
        }
        System.out.println();
    }

    public byte[] digestAll(byte[] bytes) {

        ByteBuffer all = ByteBuffer.wrap(bytes);
        int[] abcd = new int[] { 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476 };

        while (all.remaining() >= 64) {
            byte[] block = new byte[64];
            all.get(block);
            abcd = digestBlock(block, abcd);
        }
        
        if(all.remaining() > 56) {
            ByteBuffer remaining = ByteBuffer.allocate(64);
            remaining.put(all);
            remaining.put((byte) -128);
            abcd = digestBlock(remaining.array(), abcd);
            remaining = ByteBuffer.allocate(64);
            remaining.position(56);
            remaining.put((byte)(bytes.length << 3));
            remaining.put((byte)(bytes.length >> 5));
            remaining.put((byte)(bytes.length >> 13));
            remaining.put((byte)(bytes.length >> 21));
            abcd = digestBlock(remaining.array(), abcd);
            ByteBuffer toByte = ByteBuffer.allocate(16).putInt(abcd[0]).putInt(abcd[1]).putInt(abcd[2]).putInt(abcd[3]);
            toByte.position(0);
            byte[] ret = new byte[16];
            for(int i = 0; i < 4; i++) {
                ret[i << 2 | 3] = toByte.get();
                ret[i << 2 | 2] = toByte.get();
                ret[i << 2 | 1] = toByte.get();
                ret[i << 2] = toByte.get();
            }
            return ret;
        } else {
            ByteBuffer remaining = ByteBuffer.allocate(64);
            remaining.put(all);
            remaining.put((byte) -128);
            remaining.position(56);
            remaining.put((byte)(bytes.length << 3));
            remaining.put((byte)(bytes.length >> 5));
            remaining.put((byte)(bytes.length >> 13));
            remaining.put((byte)(bytes.length >> 21));
            
            abcd = digestBlock(remaining.array(), abcd);
            ByteBuffer toByte = ByteBuffer.allocate(16).putInt(abcd[0]).putInt(abcd[1]).putInt(abcd[2]).putInt(abcd[3]);
            toByte.position(0);
            byte[] ret = new byte[16];
            for(int i = 0; i < 4; i++) {
                ret[i << 2 | 3] = toByte.get();
                ret[i << 2 | 2] = toByte.get();
                ret[i << 2 | 1] = toByte.get();
                ret[i << 2] = toByte.get();
            }
            return ret;
        }
    }

    private int[] digestBlock(byte[] block, int[] abcd) {
        int[] x = new int[16];
        for (int i = 0; i < 16; i++) {
            ByteBuffer toInt = ByteBuffer.allocate(4).put(block[i << 2 | 3]).put(block[i << 2 | 2]).put(block[i << 2 | 1]).put(block[i << 2]);
            toInt.position(0);
            x[i] = toInt.getInt();
        }

        int a = abcd[0];
        int b = abcd[1];
        int c = abcd[2];
        int d = abcd[3];

        a = f(a, b, c, d, x[0], 3);
        d = f(d, a, b, c, x[1], 7);
        c = f(c, d, a, b, x[2], 11);
        b = f(b, c, d, a, x[3], 19);
        a = f(a, b, c, d, x[4], 3);
        d = f(d, a, b, c, x[5], 7);
        c = f(c, d, a, b, x[6], 11);
        b = f(b, c, d, a, x[7], 19);
        a = f(a, b, c, d, x[8], 3);
        d = f(d, a, b, c, x[9], 7);
        c = f(c, d, a, b, x[10], 11);
        b = f(b, c, d, a, x[11], 19);
        a = f(a, b, c, d, x[12], 3);
        d = f(d, a, b, c, x[13], 7);
        c = f(c, d, a, b, x[14], 11);
        b = f(b, c, d, a, x[15], 19);

        a = g(a, b, c, d, x[0], 3);
        d = g(d, a, b, c, x[4], 5);
        c = g(c, d, a, b, x[8], 9);
        b = g(b, c, d, a, x[12], 13);
        a = g(a, b, c, d, x[1], 3);
        d = g(d, a, b, c, x[5], 5);
        c = g(c, d, a, b, x[9], 9);
        b = g(b, c, d, a, x[13], 13);
        a = g(a, b, c, d, x[2], 3);
        d = g(d, a, b, c, x[6], 5);
        c = g(c, d, a, b, x[10], 9);
        b = g(b, c, d, a, x[14], 13);
        a = g(a, b, c, d, x[3], 3);
        d = g(d, a, b, c, x[7], 5);
        c = g(c, d, a, b, x[11], 9);
        b = g(b, c, d, a, x[15], 13);

        a = h(a, b, c, d, x[0], 3);
        d = h(d, a, b, c, x[8], 9);
        c = h(c, d, a, b, x[4], 11);
        b = h(b, c, d, a, x[12], 15);
        a = h(a, b, c, d, x[2], 3);
        d = h(d, a, b, c, x[10], 9);
        c = h(c, d, a, b, x[6], 11);
        b = h(b, c, d, a, x[14], 15);
        a = h(a, b, c, d, x[1], 3);
        d = h(d, a, b, c, x[9], 9);
        c = h(c, d, a, b, x[5], 11);
        b = h(b, c, d, a, x[13], 15);
        a = h(a, b, c, d, x[3], 3);
        d = h(d, a, b, c, x[11], 9);
        c = h(c, d, a, b, x[7], 11);
        b = h(b, c, d, a, x[15], 15);

        return new int[] { abcd[0] + a, abcd[1] + b, abcd[2] + c, abcd[3] + d };
    }

    private int f(int a, int b, int c, int d, int x, int s) {
        int t = a + ((b & c) | (~b & d)) + x;
        return t << s | t >>> (32 - s);
    }

    private int g(int a, int b, int c, int d, int x, int s) {
        int t = a + ((b & (c | d)) | (c & d)) + x + 0x5A827999;
        return t << s | t >>> (32 - s);
    }

    private int h(int a, int b, int c, int d, int x, int s) {
        int t = a + (b ^ c ^ d) + x + 0x6ED9EBA1;
        return t << s | t >>> (32 - s);
    }

}