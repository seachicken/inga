package p1

import p1.VarargHelper

class VarargReference {
    fun method(vararg vs: VarargHelper) {
        method(vs)
    }
}
