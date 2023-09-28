package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.isBlank;

import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;

public class RequestUtils {

    private RequestUtils() {}

    public static void mergeRequestHeader(RequestEntry<? extends MockEntry> api,
            RequestEntry<? extends MockEntry> request) {
        if(isBlank(request.getUrl())) {
            request.setUrl(api.getUrl());
        }
        if(isBlank(request.getProtocol())) {
            request.setProtocol(api.getProtocol());
        }
        if(isBlank(request.getDomain())) {
            request.setDomain(api.getDomain());
        }
        if(request.getPort() == 0) {
            request.setPort(api.getPort());
        }
        if(isBlank(request.getBasePath())) {
            request.setBasePath(api.getBasePath());
        }
        if(isBlank(request.getEndpoint())) {
            request.setEndpoint(api.getEndpoint());
        }
    }
}
