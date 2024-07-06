package p1;

import java.nio.file.Path;
import java.nio.file.Paths;

public class PrivateMethodReference {
    public void method() {
        method2();
    }

    private void method2() {
    }

    public void callWithStdLib() {
        methodWithStdLib(Paths.get("a"));
    }

    private void methodWithStdLib(Path path) {
    }
}
