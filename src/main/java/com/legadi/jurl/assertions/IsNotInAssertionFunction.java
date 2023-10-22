package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.IS_NOT_IN;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class IsNotInAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return IS_NOT_IN.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "search", "value1...N" };
    }

    @Override
    public boolean apply(String[] args) {
        return !IntStream.range(1, args.length)
            .mapToObj(index -> args[index])
            .collect(Collectors.toSet())
            .contains(args[0]);
    }

}
