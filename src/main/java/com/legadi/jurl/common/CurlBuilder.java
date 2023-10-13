package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.trim;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class CurlBuilder {

    private final Map<String, String> headers = new HashMap<>();
    private final Map<String, String> formFields = new HashMap<>();

    private String url;
    private String method;
    private String file;
    private String data;

    public CurlBuilder setUrl(URL url) {
        this.url = url.toString();
        return this;
    }

    public CurlBuilder setMethod(String method) {
        this.method = "-X " + trim(method);
        return this;
    }

    public CurlBuilder setFile(String field, String path, String filename, String mineType) {
        this.file = new StringBuilder()
            .append("-F ")
            .append("\"")
            .append(trim(field))
            .append("=@")
            .append(trim(path))
            .append(isNotBlank(filename) ? ";filename=" + trim(filename) : "")
            .append(isNotBlank(mineType) ? ";type=" + trim(mineType) : "")
            .append("\"")
            .toString();
        return this;
    }

    public CurlBuilder setData(String bodyContent) {
        this.data = "--data-raw \"" + trim(bodyContent).replaceAll("\"","\\\\\"") + "\"";
        return this;
    }

    public CurlBuilder setDataBinary(String bodyFilePath) {
        this.data = "--data-binary \"@" + trim(bodyFilePath) + "\"";
        return this;
    }

    public CurlBuilder addHeader(String headerKey, String headerValue) {
        headers.put(trim(headerKey), trim(headerValue));
        return this;
    }

    public CurlBuilder addForm(String fieldName, String fieldValue) {
        formFields.put(trim(fieldName), trim(fieldValue));
        return this;
    }

    public String build() {
        return new StringBuilder("curl")
            .append(isNotBlank(method) ? " " + method : "")
            .append(buildHeaders())
            .append(buildFormFields())
            .append(isNotBlank(file) ? " " + file : "")
            .append(isNotBlank(data) ? " " + data : "")
            .append(isNotBlank(url) ? " " + url : "")
            .toString();
    }

    private String buildHeaders() {
        if(headers.isEmpty()) {
            return "";
        }
        return " " + headers.entrySet()
            .stream()
            .map(e -> "-H \"" + e.getKey() + ": " + e.getValue() + "\"")
            .collect(Collectors.joining(" "));
    }

    private String buildFormFields() {
        if(formFields.isEmpty()) {
            return "";
        }
        return " " + formFields.entrySet()
            .stream()
            .map(e -> "-F \"" + e.getKey() + "=" + e.getValue() + "\"")
            .collect(Collectors.joining(" "));
    }
}
