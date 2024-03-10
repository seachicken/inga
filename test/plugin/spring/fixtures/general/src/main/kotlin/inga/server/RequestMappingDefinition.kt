package inga.server

import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RestController

@RequestMapping("/mapping")
@RestController
class RequestMappingDefinition {
    @RequestMapping
    fun allWithNoValue() {
    }

    @RequestMapping(value = ["/{v}"], method = [RequestMethod.GET])
    fun getWithValue(@PathVariable v: String) {
    }

    @RequestMapping(value = ["/{v1}", "/{v1}/{v2}"], method = [RequestMethod.GET])
    fun getWithValues(@PathVariable v1: String, @PathVariable(required = false) v2: String) {
    }

    @RequestMapping(path = ["/{v}"], method = [RequestMethod.GET])
    fun getWithPath(@PathVariable v: String) {
    }

    @RequestMapping(path = ["/{v1}", "/{v1}/{v2}"], method = [RequestMethod.GET])
    fun getWithPaths(@PathVariable v1: String, @PathVariable(required = false) v2: String) {
    }
}
