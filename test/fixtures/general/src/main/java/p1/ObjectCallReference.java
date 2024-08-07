package p1;

public class ObjectCallReference {
    public void callMethod() {
        var client = new ObjectCallHelper();
        client.methodSelf();
        client.method();
    }

    public void callMethodChain() {
        var client = new ObjectCallHelper();
        client.methodSelf()
                .method();
    }
}
