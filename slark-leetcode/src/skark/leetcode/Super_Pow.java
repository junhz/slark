package skark.leetcode;

public class Super_Pow {

    public int superPow(int a, int[] b) {
        int[] mods = new int[11];
        mods[0] = 1;
        mods[10] = a % 1337;
        int mod = 1;
        for (int i =  b.length - 1; i >= 0; i--) {
        	mods[1] = mods[10];
        	for (int j = 2; j < 11; j++) {
        		mods[j] = (mods[j - 1] * mods[1]) % 1337;
        	}
        	mod *= mods[b[i]];
        	mod %= 1337;
        }
        return mod;
    }
	
}
