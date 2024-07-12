package p1

class LiteralReferenceKt {
    fun method() {
        method(literal)
    }

    fun method(p: String) {
    }

    companion object {
        private const val literal = "field literal"
    }
}