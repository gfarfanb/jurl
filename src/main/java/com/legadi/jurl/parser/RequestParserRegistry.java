package com.legadi.jurl.parser;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.exception.CommandException;

public class RequestParserRegistry {

    private static final List<Pair<Predicate<String>, Supplier<RequestParser<?>>>> PARSERS = new LinkedList<>();

    static {
        registerParser(HTTPRequestParser::new);
    }

    private RequestParserRegistry() {}

    public static void registerParser(String parserClass) {
        registerParser(() -> instantiate(parserClass));
    }

    public static void registerParser(Supplier<RequestParser<?>> requestParserSupplier) {
        RequestParser<?> parser = requestParserSupplier.get();
        PARSERS.add(new Pair<>(type -> parser.accepts(type), requestParserSupplier));
    }

    @SuppressWarnings("unchecked")
    public static <T extends RequestParser<?>> T findByRequestType(String requestType) {
        List<RequestParser<?>> parsers = PARSERS
            .stream()
            .filter(p -> p.getLeft().test(requestType))
            .map(Pair::getRight)
            .map(Supplier::get)
            .collect(Collectors.toCollection(ArrayList::new));

        if(parsers.isEmpty()) {
            throw new CommandException("Unable to obtain parser for request type: " + requestType);
        }

        RequestParser<?> lastParser = parsers.get(parsers.size() - 1);
        return (T) lastParser;
    }
}
