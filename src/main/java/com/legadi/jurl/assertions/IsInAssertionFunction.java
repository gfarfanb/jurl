package com.legadi.jurl.assertions;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class IsInAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return "IS_IN";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "search", "value1...N" };
    }

    @Override
    public boolean apply(String[] args) {
        return IntStream.range(1, args.length)
            .mapToObj(index -> args[index])
            .collect(Collectors.toSet())
            .contains(args[0]);
    }

}
