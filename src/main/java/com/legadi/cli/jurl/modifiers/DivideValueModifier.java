package com.legadi.cli.jurl.modifiers;

import java.math.BigDecimal;

public class DivideValueModifier extends NumberValueModifier {

    @Override
    public String name() {
        return "divide";
    }

    @Override
    public BigDecimal apply(BigDecimal left, BigDecimal right) {
        return left.divide(right);
    }
}
