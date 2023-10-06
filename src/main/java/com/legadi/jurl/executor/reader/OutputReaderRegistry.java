package com.legadi.jurl.executor.reader;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class OutputReaderRegistry {

    private static final List<Supplier<OutputReader>> READERS = new LinkedList<>();

    static {
        registerReader(JsonOutputReader::new);
    }

    public static void registerReader(String readerClass) {
        registerReader(() -> instantiate(readerClass));
    }

    public static void registerReader(Supplier<OutputReader> outputReaderSupplier) {
        READERS.add(outputReaderSupplier);
    }

    public static Optional<OutputReader> findByContentType(String contentType) {
        List<OutputReader> readers = READERS
            .stream()
            .map(Supplier::get)
            .filter(reader -> reader.accepts(contentType))
            .collect(Collectors.toCollection(ArrayList::new));

        if(readers.isEmpty()) {
            return Optional.empty();
        }

        OutputReader lastReader = readers.get(readers.size() - 1);
        return Optional.of(lastReader);
    }
}
