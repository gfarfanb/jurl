package com.legadi.jurl.executor;

import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.BodyPublisher;
import java.net.http.HttpRequest.BodyPublishers;
import java.net.http.HttpRequest.Builder;
import static java.nio.charset.StandardCharsets.UTF_8;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicBoolean;

import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.HTTPRequestEntry;
import com.legadi.jurl.model.RequestEntry;

import static com.legadi.jurl.util.RequestUtils.getOrDefault;
import static com.legadi.jurl.util.RequestUtils.isHTTP;
import static com.legadi.jurl.util.StringUtils.isBlank;
import static com.legadi.jurl.util.StringUtils.isNotBlank;
import static com.legadi.jurl.util.StringUtils.stripEnd;

public class HTTPRequestExecutor implements RequestExecutor<HTTPRequestEntry> {

    @Override
    public boolean accepts(RequestEntry request) {
        return isHTTP(request);
    }

    @Override
    public void executeRequest(HTTPRequestEntry request) throws RequestException {
        Builder requestBuilder = HttpRequest.newBuilder();

        addURI(request, requestBuilder);
        addMethod(request, requestBuilder);
        addHeaders(request, requestBuilder);

        
    }

    private void addURI(HTTPRequestEntry request, Builder requestBuilder) {
        if(request.getUrl() == null) {
            throw new RequestException(request, "HTTP resource not defined" + request.getBodyFile());
        }

        String rawUrl = stripEnd(request.getUrl().toString(), "?&");
        StringBuilder urlPart = new StringBuilder(rawUrl);

        if(rawUrl.contains("?")) {
            request.getQueryParams().entrySet()
                .forEach(param -> urlPart
                    .append('&')
                    .append(param.getKey())
                    .append('=')
                    .append(param.getValue()));
        } else {
            AtomicBoolean first = new AtomicBoolean(true);
            request.getQueryParams().entrySet()
                .forEach(param -> {
                    if(first.get()) {
                        first.set(false);
                        urlPart
                            .append(param.getKey())
                            .append('=')
                            .append(param.getValue());
                    } else {
                        urlPart
                            .append('&')
                            .append(param.getKey())
                            .append('=')
                            .append(param.getValue());
                    }
                });
        }

        try {
            requestBuilder.uri(URI.create(urlPart.toString()));
        } catch(IllegalArgumentException ex) {
            throw new RequestException(request, "Malformed HTTP resource: " + request.getUrl());
        }
    }

    private void addMethod(HTTPRequestEntry request, Builder requestBuilder) {
        if(isBlank(request.getMethod())) {
            throw new RequestException(request, "HTTP method not defined");
        }

        switch(request.getMethod()) {
            case "PUT":
            case "POST":
                requestBuilder.method(request.getMethod(), buildBody(request));
                break;
            default:
                requestBuilder.method(request.getMethod(), BodyPublishers.noBody());
        }
    }

    private void addHeaders(HTTPRequestEntry request, Builder requestBuilder) {
        if(request.getHeaders() == null) {
            return;
        }

        request.getHeaders().entrySet()
            .forEach(header -> requestBuilder.header(header.getKey(), header.getValue()));
    }

    private BodyPublisher buildBody(HTTPRequestEntry request) {
        if(isNotBlank(request.getBodyFile())) {
            try {
                return BodyPublishers.ofFile(Path.of(request.getBodyFile()));
            } catch(FileNotFoundException ex) {
                throw new RequestException(request, "body file not found: " + request.getBodyFile());
            }
        }

        if(isNotBlank(request.getBodyContent())) {
            return BodyPublishers.ofString(request.getBodyContent(), getOrDefault(request.getBodyCharset(), UTF_8));
        }

        if(request.getRequestFile() != null) {
            return BodyPublishers.ofInputStream(() -> {
                try {
                    return new DataInputStream(new FileInputStream(request.getRequestFile()));
                } catch(FileNotFoundException ex) {
                    throw new RequestException(request, "body file not found: " + request.getBodyFile());
                }
            });
        }

        return BodyPublishers.noBody();
    }
}
