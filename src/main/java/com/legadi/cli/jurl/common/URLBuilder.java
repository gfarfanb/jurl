package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.cli.jurl.common.CommonUtils.stripEnd;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

public class URLBuilder {

    private final Map<String, String> queryParams = new HashMap<>();

    private String url;

    public URLBuilder setUrl(String url) {
        this.url = stripEnd(url, "?&");
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
            urlBuilder.append(url);
        }

        if(urlBuilder.indexOf("?") > -1) {
            queryParams
                .entrySet()
                .stream()
                .filter(e -> isNotBlank(e.getKey()))
                .forEach(param -> urlBuilder
                    .append('&')
                    .append(param.getKey())
                    .append('=')
                    .append(param.getValue()));
        } else {
            AtomicBoolean first = new AtomicBoolean(true);
            queryParams
                .entrySet()
                .stream()
                .filter(e -> isNotBlank(e.getKey()))
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
