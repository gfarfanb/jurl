package com.legadi.jurl.executor;

import com.legadi.jurl.common.Named;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.RequestInput;
import com.legadi.jurl.model.ResponseEntry;

public interface RequestType<T extends RequestEntry<? extends MockEntry>, R extends ResponseEntry> extends Named {

    @Override
    default boolean allowOverride() {
        return true;
    }

    @SuppressWarnings("unchecked")
    default RequestInput<T> cast(RequestInput<?> requestInput) {
        return (RequestInput<T>) requestInput;
    }

    @SuppressWarnings("unchecked")
    default T cast(RequestEntry<? extends MockEntry> request) {
        return (T) request;
    }

    @SuppressWarnings("unchecked")
    default R cast(ResponseEntry response) {
        return (R) response;
    }
}
