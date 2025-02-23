package p1;

import java.util.Arrays;
import java.util.HashSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// https://docs.oracle.com/javase/tutorial/java/javaOO/methodreferences.html
public class MethodReferencesReference {
    public void staticMethodRef() {
        Stream.of("1").forEach(MethodReferencesReference::staticMethod);
    }

    public void instanceMethodRef() {
        Stream.of("1").forEach(this::instanceMethod);
    }

    public void instanceMethodArbitraryObjectRef() {
        Arrays.sort(new String[]{"a"}, String::compareToIgnoreCase);
    }

    public void constructorRef() {
        Stream.of("1").collect(Collectors.toCollection(HashSet::new));
    }

    private static void staticMethod(String p) {
    }

    private void instanceMethod(String p) {
    }
}
