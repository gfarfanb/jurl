package com.legadi.jurl.model;

import com.google.gson.annotations.Expose;

public class RequestEntry {

    @Expose(serialize = false, deserialize = false)
    protected String requestPath;
    @Expose(serialize = false, deserialize = false)
    protected String name;
    protected String url;
    protected String protocol;
    protected String domain;
    protected int port;
    protected String basePath;
    protected String endpoint;

    public String getRequestPath() {
        return requestPath;
    }

    public void setRequestPath(String requestPath) {
        this.requestPath = requestPath;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getProtocol() {
        return protocol;
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

    public String getDomain() {
        return domain;
    }

    public void setDomain(String domain) {
        this.domain = domain;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public String getBasePath() {
        return basePath;
    }

    public void setBasePath(String basePath) {
        this.basePath = basePath;
    }

    public String getEndpoint() {
        return endpoint;
    }

    public void setEndpoint(String endpoint) {
        this.endpoint = endpoint;
    }

    @Override
    public String toString() {
        return "RequestEntry ["
            + "requestPath=" + requestPath
            + ", name=" + name
            + ", url=" + url
            + ", protocol=" + protocol
            + ", domain=" + domain
            + ", port=" + port
            + ", basePath=" + basePath
            + ", endpoint=" + endpoint
            + "]";
    }
}
