package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.DOES_NOT_MATCH;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DoesNotMatchAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return DOES_NOT_MATCH.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "regex", "value" };
    }

    @Override
    public boolean apply(String[] args) {
        Pattern pattern = Pattern.compile(args[0]);
        Matcher matcher = pattern.matcher(args[1]);
        return !matcher.matches();
    }

}
