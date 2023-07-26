package fixtures.java;

import fixtures.kotlin.JavaReference;

public class KotlinReference {
    private JavaReference v = new JavaReference();

    public void method() {
        v.method();
    }
}
