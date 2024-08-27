package com.legadi.cli.jurl.exception;

import com.legadi.cli.jurl.model.MockEntry;
import com.legadi.cli.jurl.model.RequestEntry;

public class RequestException extends RuntimeException {

    public RequestException(RequestEntry<? extends MockEntry> request, String message) {
        super((request.getName() != null ? "[" + request.getName() + "]" : "")
            + (message != null ? " - " + message : ""));
    }
}
