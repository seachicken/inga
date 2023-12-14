package p1;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RequestMapping("/")
@RestController
public class PathVariableDefinition {
    @GetMapping("/{v}")
    public void getWithNoValue(@PathVariable String v) {
    }

    @GetMapping("/{v}")
    public void getWithSingleMember(@PathVariable("v") String v) {
    }

    @GetMapping("/{v}")
    public void getWithValue(@PathVariable(value = "v") String v) {
    }

    @GetMapping("/{v}")
    public void getWithName(@PathVariable(name = "v") String v) {
    }
}
