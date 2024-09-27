package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.isEmpty;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.trim;

import java.net.URL;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class CurlBuilder {

    private final Map<String, String> headers = new HashMap<>();
    private final Map<String, String> formFields = new HashMap<>();
    private final List<String> files = new ArrayList<>();

    private String url;
    private String method;
    private String data;

    public CurlBuilder setUrl(URL url) {
        this.url = "\"" + url + "\"";
        return this;
    }

    public CurlBuilder setMethod(String method) {
        this.method = "-X " + trim(method);
        return this;
    }

    public CurlBuilder addFile(String field, String path, String filename, String mineType) {
        this.files.add(new StringBuilder()
            .append("-F ")
            .append("\"")
            .append(trim(field))
            .append("=@")
            .append(trim(path))
            .append(isNotBlank(filename) ? ";filename=" + trim(filename) : "")
            .append(isNotBlank(mineType) ? ";type=" + trim(mineType) : "")
            .append("\"")
            .toString()
        );
        return this;
    }

    public CurlBuilder setDataBinary(Path bodyFilePath) {
        this.data = "--data-binary \"@" + trim(bodyFilePath.toString()) + "\"";
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
            .append(build(headers, "-H", ": "))
            .append(build(formFields, "-F", "="))
            .append(files.stream().map(file -> " " + file).collect(Collectors.joining("")))
            .append(isNotBlank(data) ? " " + data : "")
            .append(isNotBlank(url) ? " " + url : "")
            .toString();
    }

    private String build(Map<String, String> elements, String opt, String separator) {
        if(isEmpty(elements)) {
            return "";
        }
        return " " + elements.entrySet()
            .stream()
            .map(e -> new StringBuilder()
                .append(opt)
                .append(" \"")
                .append(e.getKey())
                .append(separator)
                .append(e.getValue())
                .append("\"")
                .toString()
            )
            .collect(Collectors.joining(" "));
    }
}
