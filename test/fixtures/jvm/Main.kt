package jvm

import jvm.kotlin.a.Class

class Main {
    fun method() {
        Class().method()
        jvm.kotlin.b.Class().method()
    }
}
