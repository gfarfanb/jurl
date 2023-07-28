package com.legadi.jurl.options;

public abstract class RequestOption {

    protected final RequestOption option;

    public RequestOption(RequestOption option) {
        this.option = option;
    }

    public abstract void onRequest();
}
