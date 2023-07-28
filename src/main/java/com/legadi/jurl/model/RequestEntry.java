package com.legadi.jurl.model;

import java.net.URL;

public class RequestEntry {

    protected String name;
    protected String source;
    protected URL url;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public URL getUrl() {
        return url;
    }

    public void setUrl(URL url) {
        this.url = url;
    }

}
