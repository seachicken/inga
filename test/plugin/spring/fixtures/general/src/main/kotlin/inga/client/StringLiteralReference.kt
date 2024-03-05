package inga.client

import inga.client.StringLiteralHelper.WebClient

class StringLiteralReference {
    fun get() {
        WebClient("/kotlin/dummy").post()
    }

    fun method() {
        WebClient("/kotlin/string-literal-reference").get()
    }
}
