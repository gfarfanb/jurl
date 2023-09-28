package com.legadi.jurl.exception;

import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;

public class RequestException extends RuntimeException {

    public RequestException(RequestEntry<? extends MockEntry> request, String message) {
        super((request.getName() != null ? "[" + request.getName() + "] " : "") + message);
    }
}
