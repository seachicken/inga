package pkt1

import pkt1.EnumHelper.Enum

class EnumReference(private val h: EnumHelper) {
    fun method() {
        h.method(Enum.A)
    }
}