package com.legadi.jurl.assertions;

import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.model.AssertionType.IS_BLANK;

public class IsBlankAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return IS_BLANK.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) {
        return isBlank(args[0]);
    }

}
