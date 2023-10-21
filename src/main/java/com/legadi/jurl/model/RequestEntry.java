package com.legadi.jurl.model;

public class RequestEntry<T extends MockEntry> {

    protected String name;
    protected String url;
    protected String protocol;
    protected String domain;
    protected String port;
    protected String basePath;
    protected String endpoint;
    protected T mockDefinition;

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

    public String getPort() {
        return port;
    }

    public void setPort(String port) {
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

    public T getMockDefinition() {
        return mockDefinition;
    }

    public void setMockDefinition(T mockDefinition) {
        this.mockDefinition = mockDefinition;
    }

    @Override
    public String toString() {
        return "RequestEntry ["
            + "name=" + name
            + ", url=" + url
            + ", protocol=" + protocol
            + ", domain=" + domain
            + ", port=" + port
            + ", basePath=" + basePath
            + ", endpoint=" + endpoint
            + "]";
    }
}
