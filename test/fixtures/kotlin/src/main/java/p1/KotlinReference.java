package p1;

public class KotlinReference {
    private JavaReference v = new JavaReference(this);

    public void method() {
        v.method();
    }
}
