package skark.leetcode;

public class Kth_Smallest_Element_in_a_Sorted_Matrix {

    public int kthSmallest(int[][] matrix, int k) {
        int min = matrix[0][0] - 1;
        int n = matrix.length;
        int max = matrix[n - 1][n - 1] + 1;
        while (max - min > 1) {
        	int v = (max - min) / 2 + min;
        	int less = 0;
            for (int i = 0, right = n; i < n; i++) {
            	int left = -1;
            	while (right - left > 1) {
            		int p = (left + right) / 2;
            		if (matrix[i][p] < v) {
            			left = p;
            		} else {
            			right = p;
            		}
            	}
            	less += (left + 1);
            }
            if (less < k) {
            	min = v;
            } else {
            	max = v;
            }
        }
    	return min;
    }
}
