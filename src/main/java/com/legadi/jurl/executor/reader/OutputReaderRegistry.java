package com.legadi.jurl.executor.reader;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;

public class OutputReaderRegistry {

    private static final List<Pair<Predicate<String>, Supplier<OutputReader>>> READERS = new LinkedList<>();

    static {
        registerReader(JsonOutputReader::new);
    }

    private OutputReaderRegistry() {}

    public static void registerReader(String readerClass) {
        registerReader(() -> instantiate(readerClass));
    }

    public static void registerReader(Supplier<OutputReader> outputReaderSupplier) {
        OutputReader reader = outputReaderSupplier.get();
        READERS.add(new Pair<>(type -> reader.accepts(type), outputReaderSupplier));
    }

    public static Optional<OutputReader> findByContentType(String contentType) {
        List<OutputReader> readers = READERS
            .stream()
            .filter(p -> p.getLeft().test(contentType))
            .map(Pair::getRight)
            .map(Supplier::get)
            .collect(Collectors.toCollection(ArrayList::new));

        if(readers.isEmpty()) {
            return Optional.empty();
        }

        OutputReader lastReader = readers.get(readers.size() - 1);
        return Optional.of(lastReader);
    }
}
