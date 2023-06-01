package jvm.java;

import jvm.kotlin.a.Class;

public class Class {
    private Class classA;

    public void method() {
        classA.method();
        new Class().method();
        method2();
    }

    public void method2() {
        new Class().method();
    }

    public Class variable() {
        Class variable = new Class();
        return variable;
    }
}
