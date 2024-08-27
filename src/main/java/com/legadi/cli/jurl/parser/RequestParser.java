package com.legadi.cli.jurl.parser;

import java.nio.file.Path;

import com.legadi.cli.jurl.common.Evaluable;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.MockEntry;
import com.legadi.cli.jurl.model.RequestEntry;
import com.legadi.cli.jurl.model.RequestInput;

public interface RequestParser<T extends RequestEntry<? extends MockEntry>> extends Evaluable {

    @Override
    default boolean accepts(String name) {
        return type().equalsIgnoreCase(name);
    }

    String type();

    RequestInput<T> parseInput(Settings settings, Path requestPath);

    T parseRequest(Settings settings, Path requestPath);
}
