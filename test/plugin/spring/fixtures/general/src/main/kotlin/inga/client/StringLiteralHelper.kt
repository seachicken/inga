package inga.client

import org.springframework.http.HttpMethod
import org.springframework.web.client.RestTemplate

class StringLiteralHelper(
    val restTemplate: RestTemplate
) {
    @JvmInline
    value class WebClient(val path: String)

    fun WebClient.get() {
        restTemplate.exchange("${this.path}", HttpMethod.GET, null, String::class.java)
    }
    
    fun WebClient.post() {
        restTemplate.exchange("${this.path}", HttpMethod.POST, null, String::class.java)
    }
}
