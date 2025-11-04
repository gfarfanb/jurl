package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.common.CommonUtils.isBlank;

import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "IS_BLANK")
public class IsBlankAssertionFunction implements AssertionFunction {

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) {
        return isBlank(args[0]);
    }

}
