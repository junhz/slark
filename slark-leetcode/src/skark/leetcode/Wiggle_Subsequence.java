package skark.leetcode;

public class Wiggle_Subsequence {

    public int wiggleMaxLength(int[] nums) {
    	if (nums.length > 0) {
    		int last = nums[0];
            int direction = 0;
            int count = 1;
            for (int i = 1; i < nums.length; i++) {
            	if (nums[i] > last) {
            		if (direction > 0) {}
            		else {
            			count++;
            			direction = 1;
            		}
            	} else if (nums[i] < last) {
            		if (direction < 0) {}
            		else {
            			count++;
            			direction = -1;
            		}
            	} else {}
            	last = nums[i];
            }
            return count;
    	} else return 0;
    }
	
}
