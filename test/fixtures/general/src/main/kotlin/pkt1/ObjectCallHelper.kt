package pkt1

class ObjectCallHelper {
    fun method() {
    }

    fun methodSelf(): ObjectCallHelper {
        return this;
    }

    fun methodSelfWithNullable(): ObjectCallHelper? {
        return null;
    }

    fun methodWithHigherOrderFunctions(function: () -> Unit) {
        function()
    }
}