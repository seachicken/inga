package jvm.java.client;

import org.springframework.web.client.RestTemplate;

public class ClientRestTemplate {
    private final RestTemplate restTemplate;

    public ClientRestTemplate(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    public String get() {
        return restTemplate.getForObject("http://localhost:8080/path", String.class);
    }

    public String get2() {
        return restTemplate.getForObject("http://localhost:8080/path2", String.class);
    }
}
