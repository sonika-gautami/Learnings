package geeksforgeeks.datastructures;

import geeksforgeeks.datastructures.algos.GCD;

import java.util.Arrays;
import java.util.stream.IntStream;

/**
 * Given an array arr[] of N integers.
 * <p>
 * Do the following operation n-1 times. For every Kth operation:
 * Right rotate the array clockwise by 1.
 * Delete the (n-k+1)th last element.
 * <p>
 * Now, find the element which is left at last.
 */

public class ArrayRotate {

    public static void print(int[] arr, String str) {
        System.out.println(str + ": " + Arrays.toString(arr));
    }

    //Time Complexity: O(arrLen*RotationValue)
    //Auxiliary Space : O(1) ;Memory: one int temp memory block
    public static void rightRotate(int[] arr, int n) {
        print(arr, "\nGiven");
        for (int r = 0; r < n; r++) {
            int temp = arr[arr.length - 1];
            for (int i = arr.length - 1; i > 0; i--) {
                arr[i] = arr[i - 1];
            }
            arr[0] = temp;
        }
        print(arr, "rightRotate  By " + n);
    }

    //Time Complexity: O(arrLen*RotationValue)
    //Auxiliary Space : O(1) ;Memory: one int temp memory block
    public static void leftRotate(int[] arr, int n) {
        print(arr, "\nGiven");
        for (int r = 0; r < n; r++) {
            int temp = arr[0];
            for (int i = 0; i < arr.length - 1; i++) {
                arr[i] = arr[i + 1];
            }
            arr[arr.length - 1] = temp;
        }
        print(arr, "leftRotate  By " + n);
    }


    //Time Complexity: O(arrLen)
    //Auxiliary Space : O(RotationValue)
    public static void rightRotate2(int[] arr, int n) {
        print(arr, "\nGiven");

        int[] temp = new int[n];

        for (int i = 0; i < n; i++) {
            temp[i] = arr[arr.length - n + i];
        }
        for (int i = arr.length - 1; i >= n; i--) {
            arr[i] = arr[i - n];
        }
        for (int i = 0; i < n; i++) {
            arr[i] = temp[i];
        }
        System.out.println("arr2: " + Arrays.toString(arr));

        print(arr, "rightRotate2 By " + n);
    }

    //Using Cyclic Replacements
    public static void leftRotate3(int[] arr, int n) {
        print(arr, "\nGiven");
        int l = arr.length;
        n = n % l;

        int lastReplaceVal = arr[n];
        int index = n;
        for (int i = 0; i < l; i++) {
            //right rotate:
            //int newIndex = (index == 0 ? index + n : (index + n) % l);
            //left rotate:
            int newIndex = (index - n) < 0 ? index - n + l : index - n;
            int temp = arr[newIndex];

            arr[newIndex] = lastReplaceVal;
            lastReplaceVal = temp;
            index = newIndex;
        }

        print(arr, "leftRotate3 By " + n);
    }

    //Using Reverse:
    /* By2
     12345  -> 45123
     54321
     45321
     45123
     */
    public static void rightRotate4(int[] arr, int n) {
        print(arr, "\nGiven");

        reverse(arr, 0, arr.length - 1);
        reverse(arr, 0, n - 1);
        reverse(arr, n, arr.length - 1);
        print(arr, "rightRotate4 By " + n);
    }

    private static void reverse(int[] arr, int start, int end) {
        double noOftimes = Math.ceil((end - start) / 2.0);
        for (int i = 0; i < noOftimes; i++) {
            int temp = arr[i + start];
            arr[i + start] = arr[end - i];
            arr[end - i] = temp;
        }
    }


    //METHOD 3 (A Juggling Algorithm)
    //Time complexity : O(n)
    //Auxiliary Space : O(1)
    /*
    arr.l = 6, n = 2

    gcd = 2 => loop -> 0, 1

    0th  In -> 123456
    012345
    123416 t=5 j
    125416 t=3
    325416 t=1

    1st  In -> 325416
    325412 t=6
    325612 t=4
    345612 t=2
     */
    public static void leftRotate5(int[] arr, int n) {
        print(arr, "\nGiven");

        int l = arr.length;
        int gcd = GCD.gcd2(l, n);

        for (int i = 0; i < gcd; i++) {

            int j = i;
            int item = arr[j];
            int index = j - n < 0 ? j - n + l : j - n;
            do {
                int temp = arr[index];
                arr[index] = item;
                item = temp;
                j = index;
                index = j - n < 0 ? j - n + l : j - n;
            } while (j != i);
        }

        print(arr, "leftRotate5 By " + n);
    }


    public static void apply(int[] arr) {
        int l = arr.length;
        int n = l - 1;

        for (int k = 1; k < n; k++) {
            for (int i = 0; i < k; i++) {

            }
        }
    }

    public static void main(String[] args) {
        IntStream.range(1, 5).forEach(i -> {
            //RightRotate By 2: [5, 6, 1, 2, 3, 4]
            rightRotate(new int[]{1, 2, 3, 4, 5}, i);
            rightRotate2(new int[]{1, 2, 3, 4, 5}, i);
            rightRotate4(new int[]{1, 2, 3, 4, 5}, i);

            //LeftRotate By 2: [3, 4, 5, 6, 1, 2]
            leftRotate3(new int[]{1, 2, 3, 4, 5, 6}, i);
            leftRotate(new int[]{1, 2, 3, 4, 5, 6}, i);
            leftRotate5(new int[]{1, 2, 3, 4, 5, 6}, i);
        });

        /*Scanner s = new Scanner(System.in);
        int tCases = s.nextInt();
        IntStream.range(1, tCases).forEach(i -> {
            int arrLen = s.nextInt();
            apply(
                    Arrays.stream(
                            s.nextLine().split("\\s+")).limit(arrLen)
                            .mapToInt(Integer::parseInt)
                            .toArray()
            );
        });*/
    }
}
