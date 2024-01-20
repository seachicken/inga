package p1;

import java.util.Arrays;

public class ArrayReference {
    public void method() {
        String[] array = {"a", "b"};
        Arrays.copyOfRange(array, 1, array.length);
    }
}
