package main;

public enum NTLMFlags {
    
    NEGOTIATE_UNICODE("Negotiate Unicode", (byte)0),
    NEGOTIATE_OEM("Negotiate OEM", (byte)1),
    REQUEST_TARGET("Request Target", (byte)2),
    NEGOTIATE_NTLM("Negotiate NTLM", (byte)9),
    //TARGET_TYPE_DOMAIN("Target Type Domain", (byte)16),
    //TARGET_TYPE_SERVER("Target Type Server", (byte)17),
    NEGOTIATE_NTLM2_KEY("Negotiate NTLM2 Key", (byte)19),
    NEGOTIATE_TARGET_INFO("Negotiate Target Info", (byte)23)
    ;
    private final String flagName;
    private final byte flagBit;
    
    NTLMFlags(String flagName, byte flagBit) {
    	this.flagName = flagName;
    	this.flagBit = flagBit;
    }
    
    public String flagName() {
    	return flagName;
    }
    
    public byte flagValue() {
    	return flagBit;
    }
    
    public static boolean has(int flags, NTLMFlags flag) {
    	return ((flags >> flag.flagBit) & 0x01) == 1; 
    }
    
    public static boolean supportUnicode(int flags) {
    	return has(flags, NEGOTIATE_UNICODE);
    }
    
    public static boolean supportOEM(int flags) {
    	return has(flags, NEGOTIATE_OEM);
    }
    
    public static boolean echoTargetInfo(int flags) {
    	return has(flags, NEGOTIATE_TARGET_INFO);
    }
    
    public static int intValue(Iterable<NTLMFlags> flags) {
    	int ret = 0;
    	for(NTLMFlags flag : flags) {
    		ret |= 1 << flag.flagBit;
    	}
    	return ret;
    }
    
    public static int intValue(NTLMFlags... flags) {
    	int ret = 0;
    	for(NTLMFlags flag : flags) {
    		ret |= 1 << flag.flagBit;
    	}
    	return ret;
    }
    
}
