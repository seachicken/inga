package p1;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RequestMapping("/mapping")
@RestController
public class RequestMappingDefinition {
    @RequestMapping
    public void allWithNoValue() {
    }

    @RequestMapping(value = "/{v}", method = RequestMethod.GET)
    public void getWithValue(@PathVariable String v) {
    }

    @RequestMapping(path = "/{v}", method = RequestMethod.GET)
    public void getWithPath(@PathVariable String v) {
    }
}
