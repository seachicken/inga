package jvm.java;

public class Overload {
    public String method(int i) {
        return method("a");
    }

    public String method(String s) {
      return s;
    }
}
