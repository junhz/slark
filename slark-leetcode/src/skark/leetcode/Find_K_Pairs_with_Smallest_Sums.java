package skark.leetcode;

import java.util.List;

public class Find_K_Pairs_with_Smallest_Sums {

	public int kthSmallestSum(int[] a, int[] b, int k) {
		int m = a.length;
		int n = b.length;
		int min = a[0] + b[0] - 1;
		int max = a[m - 1] + b[n - 1] + 1;
		while (max - min > 1) {
			int less = 0;
			int v = (max - min) / 2 + min;
			for (int i = 0, right = n; i < m; i++) {
				int left = -1;
				while (right - left > 1) {
					int p = (left + right) / 2;
					if (a[i] + b[p] < v) {
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
	
	public List<int[]> kSmallestPairs(int[] nums1, int[] nums2, int k) {
		int m = nums1.length;
		int n = nums2.length;
		
		if (k > m * n) {
			k = m * n;
		}
		if (k > 0) {
			int sum;
			if (m > n) sum = kthSmallestSum(nums2, nums1, k);
			else sum = kthSmallestSum(nums1, nums2, k);
			
			int[][] result = new int[k][2];
			int lessCount = 0;
			int equalCount = 0;
			for (int i = 0; i < m; i++) {
				for (int j = 0; j < n; j++) {
					if (nums1[i] + nums2[j] < sum) {
						result[lessCount++] = new int[] { nums1[i], nums2[j] };
					} else if (nums1[i] + nums2[j] == sum && equalCount + lessCount  < k) {
						equalCount++;
						result[k - equalCount] = new int[] { nums1[i], nums2[j] };
					} else break;
				}
			}
			return java.util.Arrays.asList(result);
		} else return java.util.Arrays.asList();
	}
	
	/*public List<int[]> kSmallestPairs(int[] a, int[] b, int k) {
        k = Math.min(k, a.length * b.length);
        if (k > 0) {
            int[] aw = new int[a.length];
            int[] bw = new int[b.length];
            int aTail = 0;
            int bTail = 0;
            aw[0] = 1;
            bw[0] = 1;
            for (int i = 1; i < a.length; i++) {
                if (a[i] != a[i - 1]) a[++aTail] = a[i];
                
                aw[aTail]++;
            }
            for (int i = 1; i < b.length; i++) {
                if (b[i] != b[i - 1]) b[++bTail] = b[i];
                
                bw[bTail]++;
            }
            int[] aws = new int[a.length];
            int[] bws = new int[b.length];
            for (int i = 1; i <= aTail; i++) {
                aws[i] = aws[i - 1] + aw[i - 1];
            }
            for (int i = 1; i <= bTail; i++) {
                bws[i] = bws[i - 1] + bw[i - 1];
            }
            
            LinkedList<int[][]> ranges = new LinkedList<int[][]>();
            ranges.add(new int[][] { new int[] { -1, aTail + 1 }, new int[] { -1, bTail + 1} });
            int min = a[0] + b[0] - 1;
            int max = a[aTail] + b[bTail] + 1;
            
            while (!ranges.isEmpty()) {
                int[][] range = ranges.poll();
                //System.out.println("a: " + range[0][0] + " -> " + range[0][1] + ", b: " + range[1][0] + " -> " + range[1][1]);
                int n = (range[0][0] + range[0][1]) / 2;
                
                int left = range[1][0];
                int right = range[1][1];
                while (right - left > 1) {
                    int m = (right + left) / 2;
                    int t = a[n] + b[m];
                    int less = aws[n] * bw[m] + aw[n] * bws[m] + aws[n] * bws[m];
                    int equal = aw[n] * bw[m];
                    
                    if (t <= min) {
                        left = m;
                    } else if ( t >= max) {
                        right = m;
                    } else {
                        for (int i = n - 1, j = m + 1; i >= 0 && j <= bTail;) {
                            int r = a[i] + b[j];
                            if (r > t) i--;
                            else {
                                if (r == t) equal += aw[i] * bw[j];
                                else less += aw[i] * bw[j];
                                
                                less += aws[i] * bw[j];
                                j++;
                            }
                        }
                        
                        for (int i = n + 1, j = m - 1; i <= aTail && j >= 0;) {
                            int r = a[i] + b[j];
                            if (r > t) j--;
                            else {
                                if (r == t) equal += aw[i] * bw[j];
                                else less += aw[i] * bw[j];
                                
                                less += aw[i] * bws[j];
                                i++;
                            }
                        }
                        
                        if (less > k) {
                            right = m;
                            max = t;
                        } else if (less + equal < k) {
                            left = m;
                            min = t;
                        } else {
                            int[][] results = new int[k][2];
                            
                            equal = k - less;
                            for (int i = 0; i <= n; i++) {
                                for (int j = 0; j <= m; j++) {
                                    if (i == n && j == m) {
                                        for (int c = 0; c < aw[i] * bw[j] && equal > 0; c++, equal--) {
                                            results[k - equal] = new int[] { a[i], b[j] };
                                        }
                                    } else {
                                        for (int c = 0; c < aw[i] * bw[j]; c++) {
                                            results[--less] = new int[] { a[i], b[j] };
                                        }
                                    }
                                }
                            }
                            
                            for (int i = n - 1, j = m + 1; i >= 0 && j <= bTail;) {
                                int r = a[i] + b[j];
                                if (r > t) i--;
                                else {
                                    if (r == t) {
                                        for (int c = 0; c < aw[i] * bw[j] && equal > 0; c++, equal--) {
                                            results[k - equal] = new int[] { a[i], b[j] };
                                        }
                                    } else {
                                        for (int c = 0; c < aw[i] * bw[j]; c++) {
                                            results[--less] = new int[] { a[i], b[j] };
                                        }
                                    }
                                    
                                    for (int h = 0; h < i; h++) {
                                        for (int c = 0; c < aw[h] * bw[j]; c++) {
                                            results[--less] = new int[] { a[h], b[j] };
                                        }
                                    }
                                    j++;
                                }
                            }
                            
                            for (int i = n + 1, j = m - 1; i <= aTail && j >= 0;) {
                                int r = a[i] + b[j];
                                if (r > t) j--;
                                else {
                                    if (r == t) {
                                        for (int c = 0; c < aw[i] * bw[j] && equal > 0; c++, equal--) {
                                            results[k - equal] = new int[] { a[i], b[j] };
                                        }
                                    } else {
                                        for (int c = 0; c < aw[i] * bw[j]; c++) {
                                            results[--less] = new int[] { a[i], b[j] };
                                        }
                                    }
                                    
                                    for (int h = 0; h < j; h++) {
                                        for (int c = 0; c < aw[i] * bw[h]; c++) {
                                            results[--less] = new int[] { a[i], b[h] };
                                        }
                                    }
                                    i++;
                                }
                            }
                            return Arrays.asList(results);
                        }
                    }
                }
                
                if (range[0][1] - n >= 2 && right - range[1][0] >= 2) {
                    ranges.add(new int[][] { new int[] { n, range[0][1] }, new int[] { range[1][0], right } });
                }
                if (n - range[0][0] >= 2 && range[1][1] - left >= 2) {
                    ranges.add(new int[][] { new int[] { range[0][0], n }, new int[] { left, range[1][1] } });
                }
            }
        }
        
        return new LinkedList<int[]>();
    }*/
	
	public static void main(String[] args) {
		new Find_K_Pairs_with_Smallest_Sums().kSmallestPairs(new int[] { 1, 7, 11 }, new int[] { 2, 4, 6 }, 3);
	}
	
}
