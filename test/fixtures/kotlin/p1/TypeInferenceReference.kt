package p1

class TypeInferenceReference {
    fun forEach() {
        listOf("a").forEach { println(it) }
    }

    fun forEachWithReference() {
        val list = listOf("a")
        list.forEach { println(it) }
    }
}
