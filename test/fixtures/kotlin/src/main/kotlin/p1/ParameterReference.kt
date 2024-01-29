package p1

class ParameterReference {
    fun method(v: ParameterHelper) {
        v.method()
    }

    fun method(v: ParameterHelper?) {
        v.method()
    }
}

