package p1

import org.springframework.web.bind.annotation.DeleteMapping
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.PutMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RequestMapping("/")
@RestController
class RestControllerDefinition {
    @GetMapping("/{v}")
    fun get(@PathVariable("v") v: String) {
    }

    @PostMapping
    fun create() {
    }

    @PutMapping("/{v}")
    fun update(@PathVariable("v") v: String) {
    }

    @DeleteMapping("/{v}")
    fun delete(@PathVariable("v") v: String) {
    }
}
