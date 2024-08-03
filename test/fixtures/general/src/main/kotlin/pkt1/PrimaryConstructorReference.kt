package pkt1

class PrimaryConstructorReference(private val v: PrimaryConstructorHelper) {
    fun method() {
        v.method()
    }
}

