package ingakt.client

import org.springframework.http.HttpMethod
import org.springframework.http.ResponseEntity
import org.springframework.web.client.RestTemplate
import org.springframework.web.util.UriComponentsBuilder

// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
class ClientRestTemplate(private val restTemplate: RestTemplate) {
    fun get(): String {
        return restTemplate.getForObject("http://localhost:8080/kotlin/path", String::class.java)
    }

    fun get2(): String {
        return restTemplate.getForObject("http://localhost:8080/kotlin/path2", String::class.java)
    }

    fun getWithUri(): String {
        val uri = UriComponentsBuilder.fromUriString("http://localhost:8080")
            .path("/kotlin/rest-template")
            .build()
            .toUri()
        return restTemplate.getForObject(uri, String::class.java)
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
