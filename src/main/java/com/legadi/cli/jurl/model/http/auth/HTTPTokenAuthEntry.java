package com.legadi.cli.jurl.model.http.auth;

import java.util.HashMap;
import java.util.Map;

public class HTTPTokenAuthEntry {

    private String tokenUrl;
    private Map<String, String> headers = new HashMap<>();
    private String grantType;
    private String grantTypeFieldName;
    private String clientId;
    private String clientIdFieldName;
    private String clientSecret;
    private String clientSecretFieldName;
    private String scope;
    private String scopeFieldName;
    private String accessTokenFieldName;
    private String expiresInFieldName;
    private String expiresInTimeUnit;
    private String tokenTypeFieldName;

    public String getTokenUrl() {
        return tokenUrl;
    }

    public void setTokenUrl(String tokenUrl) {
        this.tokenUrl = tokenUrl;
    }

    public Map<String, String> getHeaders() {
        return headers;
    }

    public void setHeaders(Map<String, String> headers) {
        this.headers = headers;
    }

    public String getGrantType() {
        return grantType;
    }

    public void setGrantType(String grantType) {
        this.grantType = grantType;
    }

    public String getGrantTypeFieldName() {
        return grantTypeFieldName;
    }

    public void setGrantTypeFieldName(String grantTypeFieldName) {
        this.grantTypeFieldName = grantTypeFieldName;
    }

    public String getClientId() {
        return clientId;
    }

    public void setClientId(String clientId) {
        this.clientId = clientId;
    }

    public String getClientIdFieldName() {
        return clientIdFieldName;
    }

    public void setClientIdFieldName(String clientIdFieldName) {
        this.clientIdFieldName = clientIdFieldName;
    }

    public String getClientSecret() {
        return clientSecret;
    }

    public void setClientSecret(String clientSecret) {
        this.clientSecret = clientSecret;
    }

    public String getClientSecretFieldName() {
        return clientSecretFieldName;
    }

    public void setClientSecretFieldName(String clientSecretFieldName) {
        this.clientSecretFieldName = clientSecretFieldName;
    }

    public String getScope() {
        return scope;
    }

    public void setScope(String scope) {
        this.scope = scope;
    }

    public String getScopeFieldName() {
        return scopeFieldName;
    }

    public void setScopeFieldName(String scopeFieldName) {
        this.scopeFieldName = scopeFieldName;
    }

    public String getAccessTokenFieldName() {
        return accessTokenFieldName;
    }

    public void setAccessTokenFieldName(String accessTokenFieldName) {
        this.accessTokenFieldName = accessTokenFieldName;
    }

    public String getExpiresInFieldName() {
        return expiresInFieldName;
    }

    public void setExpiresInFieldName(String expiresInFieldName) {
        this.expiresInFieldName = expiresInFieldName;
    }

    public String getExpiresInTimeUnit() {
        return expiresInTimeUnit;
    }

    public void setExpiresInTimeUnit(String expiresInTimeUnit) {
        this.expiresInTimeUnit = expiresInTimeUnit;
    }

    public String getTokenTypeFieldName() {
        return tokenTypeFieldName;
    }

    public void setTokenTypeFieldName(String tokenTypeFieldName) {
        this.tokenTypeFieldName = tokenTypeFieldName;
    }
}
