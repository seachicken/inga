package inga.client;

import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.util.UriComponentsBuilder;
import reactor.core.publisher.Mono;

public class ClientWebClient {
    public Mono<String> get() {
        WebClient client = WebClient.create("http://localhost:8080");
        return client.get()
                .uri("/web-client")
                .retrieve()
                .bodyToMono(String.class);
    }

    public Mono<String> getWithUri() {
        WebClient client = WebClient.create();
        var uri = UriComponentsBuilder.fromUriString("http://localhost:8080")
                .path("/web-client")
                .build()
                .toUri();
        return client.get()
                .uri(uri)
                .retrieve()
                .bodyToMono(String.class);
    }
}
