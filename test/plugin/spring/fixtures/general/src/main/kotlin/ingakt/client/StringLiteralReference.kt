package ingakt.client

import org.springframework.web.client.RestTemplate

class StringLiteralReference(
    restTemplate: RestTemplate
) : StringLiteralHelper(restTemplate) {
    fun get() {
        WebClient("/kotlin/dummy").post()
    }

    fun method() {
        WebClient("/kotlin/string-literal-reference").get()
    }
}
