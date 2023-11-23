package com.legadi.jurl.model.http;

public class HTTPRequestAuthEntry {

    private String requestInputPath;
    private String inputName;
    private String authType;
    private String tokenParam;
    private String usernameParam;
    private String passwordParam;

    public String getRequestInputPath() {
        return requestInputPath;
    }

    public void setRequestInputPath(String requestInputPath) {
        this.requestInputPath = requestInputPath;
    }

    public String getInputName() {
        return inputName;
    }

    public void setInputName(String inputName) {
        this.inputName = inputName;
    }

    public String getAuthType() {
        return authType;
    }

    public void setAuthType(String authType) {
        this.authType = authType;
    }

    public String getTokenParam() {
        return tokenParam;
    }

    public void setTokenParam(String tokenParam) {
        this.tokenParam = tokenParam;
    }

    public String getUsernameParam() {
        return usernameParam;
    }

    public void setUsernameParam(String usernameParam) {
        this.usernameParam = usernameParam;
    }

    public String getPasswordParam() {
        return passwordParam;
    }

    public void setPasswordParam(String passwordParam) {
        this.passwordParam = passwordParam;
    }
}
