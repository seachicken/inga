package inga.server

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RequestMapping("/kotlin")
@RestController
class RestControllerDefinition {
    @GetMapping("/{v}")
    fun get(@PathVariable("v") v: String) {
    }
}
