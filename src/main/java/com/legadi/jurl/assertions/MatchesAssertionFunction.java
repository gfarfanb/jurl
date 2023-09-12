package com.legadi.jurl.assertions;

import com.legadi.jurl.exception.AssertionException;

import static com.legadi.jurl.model.AssertionType.MATCHES;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class MatchesAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return MATCHES.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "regex", "value" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        Pattern pattern = Pattern.compile(args[0]);
        Matcher matcher = pattern.matcher(args[1]);
        return matcher.matches();
    }

}
