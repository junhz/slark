package skark.leetcode;

public class Guess_Number_Higher_or_Lower_II {
	private int[] offsets;
    private int[] times;
    
    public int offsets(int stage) {
        if (stage != 0 && offsets[stage] == 0) {
        	int left = 1 << (times(stage) - 2);
        	int right = stage - left;
        	int offset;
        	if (times(left) > times(right)) {
        		right = 1 << (times(stage) - 3);
        		left = stage - right;
                offset = times(stage) * right * 4 - 1 + offsets[left];
            } else {
                offset = right * 4 - 1 + offsets[right];
            }
            offsets[stage] = offset;
        }
        return offsets[stage];
    }
    
    public int times(int stage) {
        if (stage != 0 && times[stage] == 0) {
            int time = 1;
            int mask = 1;
            while (stage > mask) {
                mask <<= 1;
                time++;
            }
            times[stage] = time;
        }
        return times[stage];
    }
    
    public int getMoneyAmount(final int i) {
        offsets = new int[i + 3];
        offsets[1] = 1;
        times = new int[i + 3];
        
        int[] values = new int[i + 3];
        values[2] = 1;
        values[3] = 2;
        int stage = 1;
        for (int j = 4; j <= i; j++) {
            int cut = j - (stage << 2) + 1;
            int lhs = values[cut - 1];
            int rhs = times(stage) * j - offsets(stage);
            int value = Math.max(lhs, rhs) + cut;
            
            int newStage = stage + 1;
            while (times(newStage + 1) == times(newStage) && offsets(newStage + 1) == offsets(newStage)) {
            	newStage++;
            }
            cut = j - (newStage << 2) + 1;
            if (cut > 0) {
                lhs = values[cut - 1];
                rhs = times(newStage) * j - offsets(newStage);
                int v = Math.max(lhs, rhs) + cut;
                if (v < value) {
                    value = v;
                    stage = newStage;
                }
            }

            //System.out.println(j + " = " + value);
            values[j] = value;
        }
        return values[i];
    }
}
