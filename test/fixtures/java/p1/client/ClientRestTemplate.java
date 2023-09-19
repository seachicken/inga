package p1.client;

import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/web/client/RestTemplate.html
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

    public ResponseEntity<String> exchangeGet() {
        return restTemplate.exchange("http://localhost:8080/path", HttpMethod.GET, null, String.class);
    }

    public String post() {
        return restTemplate.postForObject(
            "http://localhost:8080/path",
            new RequestClass("a"),
            String.class
        );
    }
}
