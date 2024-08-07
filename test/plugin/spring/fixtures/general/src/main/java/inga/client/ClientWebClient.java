package inga.client;

import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

public class ClientWebClient {
    public Mono<String> get() {
        WebClient client = WebClient.create("http://localhost:8080");
        return client.get()
                .uri("/web-client")
                .retrieve()
                .bodyToMono(String.class);
    }
}
