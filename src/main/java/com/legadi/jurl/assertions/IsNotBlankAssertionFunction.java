package com.legadi.jurl.assertions;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.AssertionType.IS_NOT_BLANK;

import com.legadi.jurl.exception.AssertionException;

public class IsNotBlankAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return IS_NOT_BLANK.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return isNotBlank(args[0]);
    }

}
