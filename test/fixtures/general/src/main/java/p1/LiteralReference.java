package p1;

public class LiteralReference {
    private static final String literal = "field literal";

    public void method() {
        method(literal);
    }

    public void method(String p) {
    }
}
