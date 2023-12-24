package p1

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RestController

@RequestMapping("/")
@RestController
class PathVariableDefinition {
    @GetMapping("/{v}")
    fun getWithNoValue(@PathVariable v: String) {
    }

    @GetMapping("/{v}")
    fun getWithSingleMember(@PathVariable("v") a: String) {
    }

    @GetMapping("/{v}")
    fun getWithValue(@PathVariable(value = "v") a: String) {
    }

    @GetMapping("/{v}")
    fun getWithName(@PathVariable(name = "v") a: String) {
    }
}
