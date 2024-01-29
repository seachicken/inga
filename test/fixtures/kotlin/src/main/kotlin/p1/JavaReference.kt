package p1

import p1.KotlinReference

class JavaReference(val v: KotlinReference) {
    fun method() {
        v.method()
    }
}

