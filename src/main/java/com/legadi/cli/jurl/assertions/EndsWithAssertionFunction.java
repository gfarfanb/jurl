package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "END_WITH")
public class EndsWithAssertionFunction implements AssertionFunction {

    @Override
    public String[] getArgs() {
        return new String[] { "value", "sufix" };
    }

    @Override
    public boolean apply(String[] args) {
        return isNotBlank(args[0])
            && isNotBlank(args[1])
            && args[0].endsWith(args[1]);
    }
}
