package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "IS_NOT_BLANK")
public class IsNotBlankAssertionFunction implements AssertionFunction {

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) {
        return isNotBlank(args[0]);
    }

}
