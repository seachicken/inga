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

    @RequestMapping(value = {"/{v1}", "/{v1}/{v2}"}, method = RequestMethod.GET)
    public void getWithValues(@PathVariable String v1, @PathVariable(required = false) String v2) {
    }

    @RequestMapping(path = "/{v}", method = RequestMethod.GET)
    public void getWithPath(@PathVariable String v) {
    }

    @RequestMapping(path = {"/{v1}", "/{v1}/{v2}"}, method = RequestMethod.GET)
    public void getWithPaths(@PathVariable String v1, @PathVariable(required = false) String v2) {
    }
}
