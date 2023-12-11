package p1;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
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

    @RequestMapping(value = "request/{v}", method = RequestMethod.GET)
    public void getWithRequest(@PathVariable("v") String v) {
    }

    @GetMapping(value = {"/{v1}/{v2}", "/{v1}"})
    public void getWithValues(
            @PathVariable String v1,
            @PathVariable(required = false) String v2
    ) {
    }

    @GetMapping("/{v}")
    public void getWithValue(@PathVariable(value = "v") String v) {
    }

    @GetMapping("/{v}")
    public void getWithName(@PathVariable(name = "v") String v) {
    }

    @GetMapping("/{v}")
    public void getWithNoValue(@PathVariable String v) {
    }
}
