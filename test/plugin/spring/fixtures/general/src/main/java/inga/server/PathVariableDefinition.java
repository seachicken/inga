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
    public void getWithSingleMember(@PathVariable("v") String a) {
    }

    @GetMapping("/{v}")
    public void getWithValue(@PathVariable(value = "v") String a) {
    }

    @GetMapping("/{v}")
    public void getWithName(@PathVariable(name = "v") String a) {
    }
}
