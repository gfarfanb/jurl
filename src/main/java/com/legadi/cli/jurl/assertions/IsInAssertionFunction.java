package com.legadi.cli.jurl.assertions;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "IS_IN")
public class IsInAssertionFunction implements AssertionFunction {

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
