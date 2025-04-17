package com.legadi.cli.jurl.model.http.auth;

import com.legadi.cli.jurl.model.AuthenticationEntry;

public class HTTPBasicAuthEntry implements AuthenticationEntry {

    private String username;
    private String password;

    @Override
    public String getParserElement() {
        return "basic";
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }
}
