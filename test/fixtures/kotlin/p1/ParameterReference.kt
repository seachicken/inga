package p1

import p1.PrimaryConstructorHelper

class ParameterReference {
    fun method(v: PrimaryConstructorHelper) {
        v.method()
    }

    fun method(v: PrimaryConstructorHelper?) {
        v.method()
    }
}

