package pkt1

class AnonymousInnerClassReference {
    fun method() {
        object : AnonymousObjectHelper() {
            override fun method() {
            }
        }
    }
}