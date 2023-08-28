package com.legadi.jurl.common;

import static com.legadi.jurl.common.StringUtils.isNotBlank;

import java.net.URL;
import java.util.LinkedList;
import java.util.List;

public class CurlBuilder {

    private final List<String> headers = new LinkedList<>();
    private final List<String> formFields = new LinkedList<>();

    private String url;
    private String method;
    private String file;
    private String data;

    public CurlBuilder setUrl(URL url) {
        this.url = url.toString();
        return this;
    }

    public CurlBuilder setMethod(String method) {
        this.method = "-X " + method;
        return this;
    }

    public CurlBuilder setFile(String field, String path, String filename, String mineType) {
        this.file = new StringBuilder()
            .append("-F ")
            .append("\"")
            .append(field)
            .append("=@")
            .append(path)
            .append(isNotBlank(filename) ? ";filename=" + filename : "")
            .append(isNotBlank(mineType) ? ";type=" + mineType : "")
            .append("\"")
            .toString();
        return this;
    }

    public CurlBuilder setData(String bodyContent) {
        this.data = "--data-raw \"" + bodyContent + "\"";
        return this;
    }

    public CurlBuilder setDataBinary(String bodyFilePath) {
        this.data = "--data-binary \"@" + bodyFilePath + "\"";
        return this;
    }

    public CurlBuilder addHeader(String header, String headerValue) {
        headers.add("-H " + header + ": " + headerValue);
        return this;
    }

    public CurlBuilder addForm(String fieldName, String fieldContent) {
        formFields.add("-F \"" + fieldName + "=" + fieldContent + "\"");
        return this;
    }

    public String build() {
        return new StringBuilder("curl")
            .append(isNotBlank(method) ? " " + method : "")
            .append(!headers.isEmpty() ? " " + String.join(" ", headers) : "")
            .append(!formFields.isEmpty() ? " " + String.join(" ", formFields) : "")
            .append(isNotBlank(file) ? " " + file : "")
            .append(isNotBlank(data) ? " " + data : "")
            .append(isNotBlank(url) ? " " + url : "")
            .toString();
    }
}
