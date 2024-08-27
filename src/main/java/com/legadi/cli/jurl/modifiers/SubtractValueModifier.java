package com.legadi.cli.jurl.modifiers;

import java.math.BigDecimal;

public class SubtractValueModifier extends NumberValueModifier {

    @Override
    public String name() {
        return "subtract";
    }

    @Override
    public BigDecimal apply(BigDecimal left, BigDecimal right) {
        return left.subtract(right);
    }
}
