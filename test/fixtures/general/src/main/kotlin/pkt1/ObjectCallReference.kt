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

    fun callMethodChainWithNullableAndHigherOrderFunctions() {
        val client = ObjectCallHelper()
        client.methodSelfWithNullable()
            ?.methodWithHigherOrderFunctions { }
    }
}