package com.legadi.cli.jurl.embedded.controller;

import static com.legadi.cli.jurl.embedded.util.RequestCatcherManager.getCatcher;

import java.util.Map;
import java.util.UUID;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.legadi.cli.jurl.embedded.model.AuthToken;
import com.legadi.cli.jurl.embedded.util.RequestCatcher;

@RestController
@RequestMapping("/oauth")
public class AuthorizationController {

    public static final long DEFAULT_EXPIRES = 5L;

    @PostMapping(value = "/token", consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE)
    public ResponseEntity<AuthToken> bearer(@RequestBody String body,
            @RequestHeader Map<String, String> requestHeaders) {
        RequestCatcher requestCatcher = getCatcher(requestHeaders.get("request-catcher"));
        UUID correlationId = UUID.randomUUID();

        requestCatcher.add(correlationId, "auth-body", body);

        AuthToken authToken = new AuthToken();
        authToken.setAccessToken(UUID.randomUUID().toString());
        authToken.setTokenType("bearer");
        authToken.setExpiresIn(DEFAULT_EXPIRES);
        authToken.setScope("jurl");

        return ResponseEntity.status(HttpStatus.OK)
            .body(authToken);
    }
}
