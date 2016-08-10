package skark.leetcode;

public class Combination_Sum_IV {

    public int combinationSum4(int[] nums, int target) {
    	int[] counts = new int[target + 1];
        for (int i = 1; i <= target; i++) {
        	for (int j = 0; j < nums.length; j++) {
        		int idx = i - nums[j];
        		if (idx == 0) counts[i]++;
        		else if (idx > 0) counts[i] += counts[idx];
        		else continue;
        	}
        }
        return counts[target];
    }
	
}
