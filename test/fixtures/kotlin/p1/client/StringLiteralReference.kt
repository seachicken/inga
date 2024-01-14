package p1.client

import p1.client.StringLiteralHelper.WebClient

class StringLiteralReference {
    fun get() {
        WebClient("/dummy").post()
    }

    fun method() {
        WebClient("/string-literal-reference").get()
    }
}
