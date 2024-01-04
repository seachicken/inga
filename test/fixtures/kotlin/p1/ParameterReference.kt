package p1

import p1.ParameterHelper

class ParameterReference {
    fun method(v: ParameterHelper) {
        v.method()
    }

    fun method(v: ParameterHelper?) {
        v.method()
    }
}

