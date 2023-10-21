package com.legadi.jurl.parser;

import java.nio.file.Path;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.RequestInput;

public interface RequestParser<T extends RequestEntry<? extends MockEntry>> {

    String type();

    default boolean accepts(String name) {
        return type().equalsIgnoreCase(name);
    }

    RequestInput<T> parseInput(Settings settings, Path requestPath);

    T parseRequest(Settings settings, Path requestPath);
}
