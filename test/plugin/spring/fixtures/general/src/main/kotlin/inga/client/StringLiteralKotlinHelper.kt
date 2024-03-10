package inga.client

import org.springframework.http.HttpMethod
import org.springframework.web.client.RestTemplate

open class StringLiteralKotlinHelper(
    val restTemplate: RestTemplate
) {
    @JvmInline
    value class WebClient(val path: String)

    protected inline fun WebClient.get() {
        restTemplate.exchange("${this.path}", HttpMethod.GET, null, String::class.java)
    }
    
    protected inline fun WebClient.post() {
        restTemplate.exchange("${this.path}", HttpMethod.POST, null, String::class.java)
    }
}
