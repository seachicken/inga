package inga.client

import org.springframework.http.HttpMethod
import org.springframework.http.ResponseEntity
import org.springframework.web.client.RestTemplate

// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
class ClientKotlinRestTemplate(private val restTemplate: RestTemplate) {
    fun get(): String {
        return restTemplate.getForObject("http://localhost:8080/kotlin/path", String::class.java)
    }

    fun get2(): String {
        return restTemplate.getForObject("http://localhost:8080/kotlin/path2", String::class.java)
    }

    fun exchangeGet(): ResponseEntity<String> {
        return restTemplate.exchange("http://localhost:8080/kotlin/path", HttpMethod.GET, null, String::class.java)
    }

    fun post(): String {
        return restTemplate.postForObject(
            "http://localhost:8080/kotlin/path",
            "data",
            String::class.java
        )
    }
}
