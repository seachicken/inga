package pkt1

class ObjectCallReference {
    fun callMethod() {
        val client = ObjectCallHelper()
        client.methodSelf()
        client.method()
    }

    fun callMethodChain() {
        val client = ObjectCallHelper()
        client.methodSelf()
            .method()
    }
}