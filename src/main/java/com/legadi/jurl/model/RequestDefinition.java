package com.legadi.jurl.model;

import java.util.Map;

public class RequestDefinition {

    private String url;
    private String protocol;
    private String domain;
    private int port;
    private String basePath;
    private String endpoint;
    private String method;
    private Map<String, String> queryParams;
    private Map<String, String> headers;
    private String charset;
    private String body;
    private String bodyFile;
    private Map<String, String> form;
    private String fileName;
    private String filePath;
    private String fileField;
    private String fileMineType;
    private AuthorizationType authType;
    private String authCredentialId; 
}
