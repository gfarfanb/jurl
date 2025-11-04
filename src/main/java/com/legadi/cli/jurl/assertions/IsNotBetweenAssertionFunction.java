package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

import java.math.BigDecimal;

import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "IS_NOT_BETWEEN")
public class IsNotBetweenAssertionFunction implements AssertionFunction {

    @Override
    public String[] getArgs() {
        return new String[] { "begin", "value", "end" };
    }

    @Override
    public boolean apply(String[] args) {
        return isNotBlank(args[0])
            && isNotBlank(args[1])
            && isNotBlank(args[2])
            && !(
                new BigDecimal(args[1]).compareTo(new BigDecimal(args[0])) >= 0
                && new BigDecimal(args[1]).compareTo(new BigDecimal(args[2])) <= 0
            );
    }

}
