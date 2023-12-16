package p1;

import org.springframework.web.bind.annotation.GetMapping;
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
}
