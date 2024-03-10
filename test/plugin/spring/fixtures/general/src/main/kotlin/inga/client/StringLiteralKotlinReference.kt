package inga.client

import org.springframework.web.client.RestTemplate

class StringLiteralKotlinReference(
    restTemplate: RestTemplate
) : StringLiteralKotlinHelper(restTemplate) {
    fun get() {
        WebClient("/kotlin/dummy").post()
    }

    fun method() {
        WebClient("/kotlin/string-literal-reference").get()
    }
}
