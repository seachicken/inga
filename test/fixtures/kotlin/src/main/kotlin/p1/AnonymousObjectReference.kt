package p1

import java.lang.Runnable
import java.lang.Thread

class AnonymousObjectReference {
    fun method() {
        Thread(object : Runnable {
            override fun run() {
            }
        }).start()
    }

    fun methodWithSuperTypeCall() {
        anonymousMethod(object : AnonymousObjectHelper() {})
    }

    private fun anonymousMethod(v: AnonymousObjectHelper) {
    }
}
