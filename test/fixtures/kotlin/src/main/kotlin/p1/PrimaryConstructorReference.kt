package p1

import p1.PrimaryConstructorHelper

class PrimaryConstructorReference(val v: PrimaryConstructorHelper) {
    fun method() {
        v.method()
    }
}

