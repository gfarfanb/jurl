package com.legadi.cli.jurl.model.http.auth;

import com.legadi.cli.jurl.model.AuthenticationEntry;

public class HTTPTokenAuthEntry implements AuthenticationEntry {

    private String tokenUrl;
    private String grantType;
    private String clientId;
    private String clientSecret;
    private String scope;

    @Override
    public String getParserElement() {
        return "token";
    }

    public String getTokenUrl() {
        return tokenUrl;
    }

    public void setTokenUrl(String tokenUrl) {
        this.tokenUrl = tokenUrl;
    }

    public String getGrantType() {
        return grantType;
    }

    public void setGrantType(String grantType) {
        this.grantType = grantType;
    }

    public String getClientId() {
        return clientId;
    }

    public void setClientId(String clientId) {
        this.clientId = clientId;
    }

    public String getClientSecret() {
        return clientSecret;
    }

    public void setClientSecret(String clientSecret) {
        this.clientSecret = clientSecret;
    }

    public String getScope() {
        return scope;
    }

    public void setScope(String scope) {
        this.scope = scope;
    }
}
