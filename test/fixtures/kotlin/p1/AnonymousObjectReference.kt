package p1

import java.lang.Runnable
import java.lang.Thread

class AnonymousObjectReference {
    fun method() {
        Thread(object : Runnable {
            override fun run() {
            }
        }).run()
    }
}
