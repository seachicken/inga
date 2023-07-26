package fixtures.kotlin

import fixtures.java.KotlinReference

class JavaReference(val v: KotlinReference) {
    fun method() {
        v.method()
    }
}

