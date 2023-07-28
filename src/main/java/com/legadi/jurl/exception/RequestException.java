package com.legadi.jurl.exception;

import com.legadi.jurl.model.RequestEntry;

public class RequestException extends RuntimeException {

    public RequestException(RequestEntry request, String message) {
        super("[" + (request.getSource() != null ? request.getSource() + "/" : "") + request.getName() + "] " + message);
    }
}
