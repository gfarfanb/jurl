package com.legadi.jurl.embedded.controller;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/basic")
public class BasicFunctionsController {

    private final Map<String, Object> data = new HashMap<>();

    @PostMapping("/body")
    public ResponseEntity<String> post(@RequestBody Map<String, Object> body) {
        String id = UUID.randomUUID().toString();

        data.put(id, body);

        HttpHeaders responseHeaders = new HttpHeaders();
        responseHeaders.set("Resource-Id", id);

        return new ResponseEntity<>(responseHeaders, HttpStatus.OK);
    }

    @GetMapping("/body/{id}")
    public ResponseEntity<Object> get(@PathVariable String id) {
        return ResponseEntity.ok(data.get(id));
    }
}
