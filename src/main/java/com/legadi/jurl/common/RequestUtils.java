package com.legadi.jurl.common;

import static com.legadi.jurl.common.StringUtils.isNotBlank;

import com.legadi.jurl.model.RequestEntry;

public class RequestUtils {

    public static final String MULTIPART_LINE_END = "\r\n";
    public static final String MULTIPART_TWO_HYPHENS = "--";
    public static final int MULTIPART_MAX_BUFFER_SIZE = 1 * 1024 * 1024;

    public static boolean isHTTP(RequestEntry request) {
        return (isNotBlank(request.getUrl()) && request.getUrl().startsWith("http"))
            || (isNotBlank(request.getProtocol()) && request.getProtocol().startsWith("http"));
    }
}
