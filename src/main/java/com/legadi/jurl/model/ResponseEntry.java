package com.legadi.jurl.model;

public class ResponseEntry {

    protected String requestUrl;
    protected String curlCommand;

    public String getRequestUrl() {
        return requestUrl;
    }

    public void setRequestUrl(String requestUrl) {
        this.requestUrl = requestUrl;
    }

    public String getCurlCommand() {
        return curlCommand;
    }

    public void setCurlCommand(String curlCommand) {
        this.curlCommand = curlCommand;
    }
}
