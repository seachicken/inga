package p1

import p1.PropertyHelper

class PropertyReference {
    val p: String

    fun method(h: PropertyHelper) {
        h.method(p)
    }
}
