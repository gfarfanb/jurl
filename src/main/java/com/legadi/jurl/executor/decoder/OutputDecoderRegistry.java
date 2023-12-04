package com.legadi.jurl.executor.decoder;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;

public class OutputDecoderRegistry {

    private static final List<Pair<Predicate<String>, Supplier<OutputDecoder>>> DECODERS = new LinkedList<>();

    static {
        registerDecoder(GzipOutputDecoder::new);
    }

    private OutputDecoderRegistry() {}

    public static void registerDecoder(String decoderClass) {
        registerDecoder(() -> instantiate(decoderClass));
    }

    public static void registerDecoder(Supplier<OutputDecoder> outputDecoderSupplier) {
        OutputDecoder decoder = outputDecoderSupplier.get();
        DECODERS.add(new Pair<>(type -> decoder.accepts(type), outputDecoderSupplier));
    }

    public static Optional<OutputDecoder> findByContentEncoding(String contentEncoding) {
        List<OutputDecoder> decoders = DECODERS
            .stream()
            .filter(p -> p.getLeft().test(contentEncoding))
            .map(Pair::getRight)
            .map(Supplier::get)
            .collect(Collectors.toCollection(ArrayList::new));

        if(decoders.isEmpty()) {
            return Optional.empty();
        }

        OutputDecoder lastDecoder = decoders.get(decoders.size() - 1);
        return Optional.of(lastDecoder);
    }
}
