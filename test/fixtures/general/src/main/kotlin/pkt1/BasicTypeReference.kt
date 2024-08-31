package pkt1

class BasicTypeReference(private val helper: BasicTypeHelper) {
    fun methodWithInt() {
        helper.method(0)
    }

    fun methodWithBoolean() {
        helper.method(true)
    }
}
