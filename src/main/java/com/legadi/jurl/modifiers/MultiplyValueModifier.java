package com.legadi.jurl.modifiers;

import java.math.BigDecimal;

public class MultiplyValueModifier extends NumberValueModifier {

    @Override
    public String name() {
        return "multiply";
    }

    @Override
    public BigDecimal apply(BigDecimal left, BigDecimal right) {
        return left.multiply(right);
    }
}
