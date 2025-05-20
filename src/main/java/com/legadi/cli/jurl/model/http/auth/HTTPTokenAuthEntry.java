package com.legadi.cli.jurl.model.http.auth;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.legadi.cli.jurl.model.AuthenticationEntry;
import com.legadi.cli.jurl.model.StringFieldEntry;

public class HTTPTokenAuthEntry implements AuthenticationEntry, StringFieldEntry {

    private String tokenUrl;
    private String grantType;
    private String clientId;
    private String clientSecret;
    private String scope;

    private Map<String, String> otherFields = new HashMap<>();

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

    public Map<String, String> getOtherFields() {
        return otherFields;
    }

    public void setOtherFields(Map<String, String> otherFields) {
        this.otherFields = otherFields;
    }

    @Override
    public void putField(String fieldName, String value) {
        otherFields.put(fieldName, value);
    }

    @Override
    public String getField(String fieldName) {
        return otherFields.get(fieldName);
    }

    @Override
    public Set<String> getFieldNames() {
        return otherFields.keySet();
    }
}
