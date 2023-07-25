package fixtures.kotlin

import fixtures.kotlin.PrimaryConstructorHelper

class PrimaryConstructorReference(val v: PrimaryConstructorHelper) {
    fun method() {
        v.method()
    }
}

