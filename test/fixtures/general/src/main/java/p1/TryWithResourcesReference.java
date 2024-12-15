package p1;

public class TryWithResourcesReference {
    public void method() {
        try (var a = new Closeable()) {
        }
    }

    private static class Closeable implements AutoCloseable {
        @Override
        public void close() {
        }
    }
}
