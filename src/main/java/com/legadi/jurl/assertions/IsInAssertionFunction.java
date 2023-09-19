package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.IS_IN;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.legadi.jurl.exception.AssertionException;

public class IsInAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return IS_IN.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "search", "value1...N" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return IntStream.range(1, args.length)
            .mapToObj(index -> args[index])
            .collect(Collectors.toSet())
            .contains(args[0]);
    }

}
