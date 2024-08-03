package pkt1

class VarargReference {
    fun method(vararg vs: VarargHelper) {
        method(*vs)
    }
}
