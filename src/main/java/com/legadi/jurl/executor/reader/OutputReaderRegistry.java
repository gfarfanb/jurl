package com.legadi.jurl.executor.reader;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class OutputReaderRegistry {

    private static final List<Reader> READERS = new LinkedList<>();

    static {
        registerReader(JsonOutputReader::new);
        registerReader(XmlOutputReader::new);
    }

    public static void registerReader(String readerClass) {
        Supplier<OutputReader> readerSupplier = () -> instantiate(readerClass);

        registerReader(readerSupplier);
    }

    public static void registerReader(Supplier<OutputReader> outputReaderSupplier) {
        OutputReader outputReader = outputReaderSupplier.get();
        Reader reader = new Reader();

        reader.setAccepts(outputReader::accepts);
        reader.setReader(outputReaderSupplier);

        READERS.add(reader);
    }

    public static Optional<OutputReader> findByContentType(String contentType) {
        List<Reader> readers = READERS
            .stream()
            .filter(reader -> reader.getAccepts().test(contentType))
            .collect(Collectors.toCollection(ArrayList::new));

        if(readers.isEmpty()) {
            return Optional.empty();
        }

        OutputReader lastReader = readers.get(readers.size() - 1).getReader().get();

        return Optional.of(lastReader);
    }

    public static class Reader {

        private Predicate<String> accepts;
        private Supplier<OutputReader> reader;

        public Predicate<String> getAccepts() {
            return accepts;
        }

        public void setAccepts(Predicate<String> accepts) {
            this.accepts = accepts;
        }

        public Supplier<OutputReader> getReader() {
            return reader;
        }

        public void setReader(Supplier<OutputReader> reader) {
            this.reader = reader;
        }
    }
}
