package inga.client;

import org.springframework.http.HttpMethod;
import org.springframework.web.client.RestTemplate;

public class StringLiteralHelper {
    public static class WebClient {
        private final String path;
        private RestTemplate restTemplate;

        public WebClient(String path) {
           this.path = path;
        }

        public void get() {
            restTemplate.exchange(path, HttpMethod.GET, null, String.class);
        }
    }
}
