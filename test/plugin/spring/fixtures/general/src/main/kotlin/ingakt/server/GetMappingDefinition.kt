package ingakt.server

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RequestMapping("/mapping")
@RestController
class GetMappingDefinition {
    @GetMapping
    fun getWithNoValue() {
    }

    @GetMapping("/{v}")
    fun getWithSingleMember() {
    }
}
