package com.legadi.jurl.embedded.controller;

import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.legadi.jurl.embedded.model.AuthToken;
import com.legadi.jurl.embedded.util.RequestCatcher;

@RestController
@RequestMapping("/oauth")
public class AuthorizationController {

    public static final long DEFAULT_EXPIRES = 3L;

    @Autowired
    private RequestCatcher requestCatcher;

    @PostMapping(value = "/token", consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE)
    public ResponseEntity<AuthToken> bearer(@RequestBody String body) {
        UUID id = UUID.randomUUID();

        requestCatcher.add(id, "auth-body", body);

        AuthToken authToken = new AuthToken();
        authToken.setAccessToken(UUID.randomUUID().toString());
        authToken.setTokenType("bearer");
        authToken.setExpiresIn(DEFAULT_EXPIRES);
        authToken.setScope("jurl");

        return ResponseEntity.status(HttpStatus.OK)
            .body(authToken);
    }
}
