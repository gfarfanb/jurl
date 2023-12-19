package com.legadi.jurl.modifiers;

import java.math.BigDecimal;
import java.util.function.Function;

public abstract class NumberValueModifier implements ValueModifier {

    @Override
    public String[] getArgs() {
        return new String[] { "input" };
    }

    @Override
    public String apply(Function<String, String> getter, String[] args, String value) throws Exception {
        BigDecimal number = new BigDecimal(value);
        BigDecimal input = new BigDecimal(getter.apply(args[0]));
        return apply(number, input).toString();
    }

    protected abstract BigDecimal apply(BigDecimal left, BigDecimal right);
}
