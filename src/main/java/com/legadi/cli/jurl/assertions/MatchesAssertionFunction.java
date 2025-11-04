package com.legadi.cli.jurl.assertions;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "MATCHES")
public class MatchesAssertionFunction implements AssertionFunction {

    @Override
    public String[] getArgs() {
        return new String[] { "regex", "value" };
    }

    @Override
    public boolean apply(String[] args) {
        Pattern pattern = Pattern.compile(args[0]);
        Matcher matcher = pattern.matcher(args[1]);
        return matcher.matches();
    }

}
