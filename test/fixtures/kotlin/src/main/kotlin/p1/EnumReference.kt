package p1

import p1.EnumHelper.Enum

class EnumReference(private val h: EnumHelper) {
    fun method() {
        h.method(Enum.A)
    }
}