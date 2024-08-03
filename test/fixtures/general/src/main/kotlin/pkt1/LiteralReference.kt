package pkt1

class LiteralReference {
    fun method() {
        method(literal)
    }

    fun method(p: String) {
    }

    companion object {
        private const val literal = "field literal"
    }
}