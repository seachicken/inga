package p1;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RequestMapping("/mapping")
@RestController
public class GetMappingDefinition {
    @GetMapping
    public void getWithNoValue() {
    }

    @GetMapping("/{v}")
    public void getWithSingleMember() {
    }

    @GetMapping(value = "/{v}")
    public void getWithValue(@PathVariable String v) {
    }

    @GetMapping(value = {"/{v1}", "/{v1}/{v2}"})
    public void getWithValues(@PathVariable String v1, @PathVariable(required = false) String v2) {
    }

    @GetMapping(path = "/{v}")
    public void getWithPath(@PathVariable String v) {
    }

    @GetMapping(path = {"/{v1}", "/{v1}/{v2}"})
    public void getWithPaths(@PathVariable String v1, @PathVariable(required = false) String v2) {
    }
}
