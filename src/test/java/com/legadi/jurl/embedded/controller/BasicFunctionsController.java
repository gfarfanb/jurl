package com.legadi.jurl.embedded.controller;

import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.embedded.model.BasicFunctionsEntity;
import com.legadi.jurl.embedded.util.RequestCatcher;

@RestController
@RequestMapping("/basic")
public class BasicFunctionsController {

    @Autowired
    private RequestCatcher requestCatcher;

    @PostMapping("/body")
    public ResponseEntity<String> post(@RequestBody BasicFunctionsEntity entity) {
        UUID id = UUID.randomUUID();

        requestCatcher.add(new TypeToken<BasicFunctionsEntity>() {}, id, entity);

        HttpHeaders headers = new HttpHeaders();
        headers.set("Resource-ID", id.toString());

        return ResponseEntity.status(HttpStatus.CREATED)
            .headers(headers)
            .body("Created");
    }

    @GetMapping("/body/{id}")
    public ResponseEntity<Object> get(@PathVariable UUID id) {
        if(!requestCatcher.contains(new TypeToken<BasicFunctionsEntity>() {}, id)) {
            return ResponseEntity.notFound().build();
        }

        return ResponseEntity.ok(requestCatcher.get(new TypeToken<BasicFunctionsEntity>() {}, id));
    }

    @PutMapping("/body/{id}")
    public ResponseEntity<String> put(@PathVariable UUID id, @RequestBody BasicFunctionsEntity entity) {
        if(!requestCatcher.contains(new TypeToken<BasicFunctionsEntity>() {}, id)) {
            return ResponseEntity.notFound().build();
        }

        requestCatcher.add(new TypeToken<BasicFunctionsEntity>() {}, id, entity);
        return ResponseEntity.noContent().build();
    }

    @DeleteMapping("/body/{id}")
    public ResponseEntity<String> delete(@PathVariable UUID id) {
        if(!requestCatcher.contains(new TypeToken<BasicFunctionsEntity>() {}, id)) {
            return ResponseEntity.notFound().build();
        }

        requestCatcher.remove(new TypeToken<BasicFunctionsEntity>() {}, id);
        return ResponseEntity.noContent().build();
    }
}
