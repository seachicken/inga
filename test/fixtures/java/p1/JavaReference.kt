package p1.kotlin

import p1.KotlinReference

class JavaReference(val v: KotlinReference) {
    fun method() {
        v.method()
    }
}

