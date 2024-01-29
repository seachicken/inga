package p1

import java.lang.Runnable
import java.lang.Thread
import org.springframework.core.ParameterizedTypeReference

class AnonymousObjectReference {
    fun method() {
        Thread(object : Runnable {
            override fun run() {
            }
        }).start()
    }

    fun methodWithSuperTypeCall(v: AnonymousObjectHelper) {
        v.method(object : ParameterizedTypeReference<T>() {})
    }
}
