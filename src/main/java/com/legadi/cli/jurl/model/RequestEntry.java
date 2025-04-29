package com.legadi.cli.jurl.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.legadi.cli.jurl.options.OptionsReader.OptionEntry;

public class RequestEntry<T extends MockEntry> {

    protected String name;
    protected String description;
    protected String url;
    protected String protocol;
    protected String host;
    protected String port;
    protected String basePath;
    protected String endpoint;
    protected T mockDefinition;

    protected List<AssertionEntry> conditions = new ArrayList<>();
    protected Map<String, String> outputMappings = new LinkedHashMap<>();
    protected List<AssertionEntry> assertions = new ArrayList<>();
    protected List<OptionEntry> options = new ArrayList<>();
    protected Map<String, Object> defaults = new LinkedHashMap<>();
    protected Map<String, AuthenticationEntry> authEntries = new HashMap<>();

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getProtocol() {
        return protocol;
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public String getPort() {
        return port;
    }

    public void setPort(String port) {
        this.port = port;
    }

    public String getBasePath() {
        return basePath;
    }

    public void setBasePath(String basePath) {
        this.basePath = basePath;
    }

    public String getEndpoint() {
        return endpoint;
    }

    public void setEndpoint(String endpoint) {
        this.endpoint = endpoint;
    }

    public T getMockDefinition() {
        return mockDefinition;
    }

    public void setMockDefinition(T mockDefinition) {
        this.mockDefinition = mockDefinition;
    }

    public List<AssertionEntry> getConditions() {
        return conditions;
    }

    public void setConditions(List<AssertionEntry> conditions) {
        this.conditions = conditions;
    }

    public Map<String, String> getOutputMappings() {
        return outputMappings;
    }

    public void setOutputMappings(Map<String, String> outputMappings) {
        this.outputMappings = outputMappings;
    }

    public List<AssertionEntry> getAssertions() {
        return assertions;
    }

    public void setAssertions(List<AssertionEntry> assertions) {
        this.assertions = assertions;
    }

    public List<OptionEntry> getOptions() {
        return options;
    }

    public void setOptions(List<OptionEntry> options) {
        this.options = options;
    }

    public Map<String, Object> getDefaults() {
        return defaults;
    }

    public void setDefaults(Map<String, Object> defaults) {
        this.defaults = defaults;
    }

    public Map<String, AuthenticationEntry> getAuthEntries() {
        return authEntries;
    }

    public void setAuthEntries(Map<String, AuthenticationEntry> authEntries) {
        this.authEntries = authEntries;
    }
}
