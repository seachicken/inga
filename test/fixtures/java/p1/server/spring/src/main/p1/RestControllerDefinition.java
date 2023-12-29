package p1;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RequestMapping("/")
@RestController
public class RestControllerDefinition {
    @GetMapping("/{v}")
    public void get(@PathVariable("v") String v) {
    }
}
