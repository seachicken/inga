package p1;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RequestMapping("/")
@RestController
public class RestControllerDefinition {
    @GetMapping("/{v}")
    public void get(@PathVariable("v") String v) {
    }

    @PostMapping
    public void create() {
    }

    @PutMapping("/{v}")
    public void update(@PathVariable("v") String v) {
    }

    @DeleteMapping("/{v}")
    public void delete(@PathVariable("v") String v) {
    }
}
