package p1;

import pkt1.JavaReference;

public class KotlinReference {
    private JavaReference v = new JavaReference(this);

    public void method() {
        v.method();
    }
}
