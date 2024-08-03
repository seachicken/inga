package pkt1

class PropertyReference {
    private val p: String = "a"

    fun method(h: PropertyHelper) {
        h.method(p)
    }
}
