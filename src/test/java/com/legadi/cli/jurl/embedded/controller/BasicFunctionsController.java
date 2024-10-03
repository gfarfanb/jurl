package com.legadi.cli.jurl.embedded.controller;

import static com.legadi.cli.jurl.embedded.util.ObjectName.BODY;
import static com.legadi.cli.jurl.embedded.util.RequestCatcherManager.getCatcher;

import java.util.Map;
import java.util.UUID;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.legadi.cli.jurl.embedded.model.BasicFunctionsEntity;
import com.legadi.cli.jurl.embedded.util.RequestCatcher;

@RestController
@RequestMapping("/basic")
public class BasicFunctionsController {

    @PostMapping("/body")
    public ResponseEntity<String> post(@RequestBody BasicFunctionsEntity entity,
            @RequestHeader Map<String, String> requestHeaders) {
        RequestCatcher requestCatcher = getCatcher(requestHeaders.get("request-catcher"));
        UUID correlationId = UUID.randomUUID();

        requestCatcher.add(correlationId, BODY, entity);

        HttpHeaders headers = new HttpHeaders();
        headers.set("Resource-ID", correlationId.toString());

        return ResponseEntity.status(HttpStatus.CREATED)
            .headers(headers)
            .body("Created");
    }

    @GetMapping("/body/{correlationId}")
    public ResponseEntity<Object> get(@PathVariable UUID correlationId,
            @RequestHeader Map<String, String> requestHeaders) {
        RequestCatcher requestCatcher = getCatcher(requestHeaders.get("request-catcher"));

        if(!requestCatcher.contains(correlationId, BODY)) {
            return ResponseEntity.notFound().build();
        }

        return ResponseEntity.ok(requestCatcher.getLast(correlationId, BODY));
    }

    @PutMapping("/body/{correlationId}")
    public ResponseEntity<String> put(@PathVariable UUID correlationId,
            @RequestBody BasicFunctionsEntity entity,
            @RequestHeader Map<String, String> requestHeaders) {
        RequestCatcher requestCatcher = getCatcher(requestHeaders.get("request-catcher"));

        if(!requestCatcher.contains(correlationId, BODY)) {
            return ResponseEntity.notFound().build();
        }

        requestCatcher.add(correlationId, BODY, entity);
        return ResponseEntity.noContent().build();
    }

    @DeleteMapping("/body/{correlationId}")
    public ResponseEntity<String> delete(@PathVariable UUID correlationId,
            @RequestHeader Map<String, String> requestHeaders) {
        RequestCatcher requestCatcher = getCatcher(requestHeaders.get("request-catcher"));

        if(!requestCatcher.contains(correlationId, BODY)) {
            return ResponseEntity.notFound().build();
        }

        requestCatcher.remove(correlationId, BODY);
        return ResponseEntity.noContent().build();
    }

    @GetMapping("/body/empty")
    public ResponseEntity<String> empty() {
        return ResponseEntity.ok("");
    }

    @PostMapping("/body/inputs")
    public ResponseEntity<String> inputs(@RequestBody Map<String, String> entity,
            @RequestHeader Map<String, String> requestHeaders) {
        RequestCatcher requestCatcher = getCatcher(requestHeaders.get("request-catcher"));
        UUID correlationId = UUID.randomUUID();

        requestCatcher.add(correlationId, BODY, entity);

        HttpHeaders headers = new HttpHeaders();
        headers.set("Resource-ID", correlationId.toString());

        return ResponseEntity.status(HttpStatus.CREATED)
            .headers(headers)
            .body("Created");
    }
}
