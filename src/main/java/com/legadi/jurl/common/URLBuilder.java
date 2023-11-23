package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.common.CommonUtils.isNotNumeric;
import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.CommonUtils.stripEnd;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import com.legadi.jurl.exception.CommandException;

public class URLBuilder {

    private final Map<String, String> queryParams = new HashMap<>();

    private String url;
    private String protocol;
    private String host;
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

    public URLBuilder setHost(String host) {
        this.host = strip(host, ":/?&");
        return this;
    }

    public URLBuilder setPort(String port) {
        if(isBlank(port)) {
            return this;
        }
        if(isNotNumeric(port)) {
            throw new CommandException("Port is not an integer: " + port);
        }
        this.port = Integer.parseInt(port);
        return this;
    }

    public URLBuilder setBasePath(String basePath) {
        this.basePath = strip(basePath, "/?&");
        return this;
    }

    public URLBuilder setEndpoint(String endpoint) {
        this.endpoint = strip(endpoint, "/?&");
        return this;
    }

    public URLBuilder addAllQueryParams(Map<String, String> queryParams) {
        if(isNotEmpty(queryParams)) {
            this.queryParams.putAll(queryParams);
        }
        return this;
    }

    public String build() {
        StringBuilder urlBuilder = new StringBuilder();

        if(isNotBlank(url)) {
            urlBuilder
                .append(url)
                .append(isNotBlank(basePath) ? "/" + basePath : "")
                .append(isNotBlank(endpoint) ? "/" + endpoint : "");
        } else {
            urlBuilder
                .append(isNotBlank(protocol) ? protocol + "://" : "")
                .append(isNotBlank(host) ? host : "")
                .append(port > 0 ? ":" + port : "")
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
                            .append('?')
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
