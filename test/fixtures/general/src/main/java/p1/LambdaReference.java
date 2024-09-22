package p1;

public class LambdaReference {
    public void method() {
        final String v = "a";
        new LambdaHelper().methodWithLambda(() -> new LambdaHelper().method(v));
    }
}
