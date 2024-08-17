package ingakt.client

import org.springframework.web.reactive.function.client.WebClient
import org.springframework.web.util.UriComponentsBuilder
import reactor.core.publisher.Mono

class ClientWebClient {
    fun get(): Mono<String> {
        val client = WebClient.create("http://localhost:8080")
        return client.get()
            .uri("/web-client")
            .retrieve()
            .bodyToMono(String::class.java)
    }

    fun getWithUri(): Mono<String> {
        val client = WebClient.create()
        val uri = UriComponentsBuilder.fromUriString("http://localhost:8080")
            .path("/web-client")
            .build()
            .toUri()
        return client.get()
            .uri(uri)
            .retrieve()
            .bodyToMono(String::class.java)
    }
}