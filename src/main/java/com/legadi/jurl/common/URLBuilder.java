package com.legadi.jurl.common;

import static com.legadi.jurl.common.StringUtils.isNotBlank;
import static com.legadi.jurl.common.StringUtils.strip;
import static com.legadi.jurl.common.StringUtils.stripEnd;
import static com.legadi.jurl.common.StringUtils.stripStart;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

public class URLBuilder {

    private final Map<String, String> queryParams = new HashMap<>();

    private String url;
    private String protocol;
    private String domain;
    private int port;
    private String basePath;
    private String endpoint;

    public URLBuilder setUrl(String url) {
        this.url = stripEnd(url, "?&");
        return this;
    }

    public URLBuilder setProtocol(String protocol) {
        this.protocol = stripEnd(protocol, ":/");
        return this;
    }

    public URLBuilder setDomain(String domain) {
        this.domain = strip(domain, "/?&");
        return this;
    }

    public URLBuilder setPort(int port) {
        this.port = port;
        return this;
    }

    public URLBuilder setBasePath(String basePath) {
        this.basePath = stripStart(basePath, "/?&");
        return this;
    }

    public URLBuilder setEndpoint(String endpoint) {
        this.endpoint = stripStart(endpoint, "/?&");
        return this;
    }

    public URLBuilder addAllQueryParams(Map<String, String> queryParams) {
        if(queryParams != null) {
            this.queryParams.putAll(queryParams);
        }
        return this;
    }

    public String build() {
        StringBuilder urlBuilder = new StringBuilder();

        if(isNotBlank(url)) {
            urlBuilder.append(url);
        } else {
            urlBuilder
                .append(isNotBlank(protocol) ? protocol : "")
                .append("://")
                .append(isNotBlank(domain) ? domain : "")
                .append(port > 0 ? ":" + port : "/")
                .append(isNotBlank(basePath) ? "/" + basePath : "")
                .append(isNotBlank(endpoint) ? "/" + endpoint : "");
        }

        if(urlBuilder.indexOf("?") > -1) {
            queryParams.entrySet()
                .forEach(param -> urlBuilder
                    .append('&')
                    .append(param.getKey())
                    .append('=')
                    .append(param.getValue()));
        } else {
            AtomicBoolean first = new AtomicBoolean(true);
            queryParams.entrySet()
                .forEach(param -> {
                    if(first.get()) {
                        first.set(false);
                        urlBuilder
                            .append(param.getKey())
                            .append('=')
                            .append(param.getValue());
                    } else {
                        urlBuilder
                            .append('&')
                            .append(param.getKey())
                            .append('=')
                            .append(param.getValue());
                    }
                });
        }

        return urlBuilder.toString();
    }
}
