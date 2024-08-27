package com.legadi.cli.jurl.modifiers;

import java.math.BigDecimal;

public class AddValueModifier extends NumberValueModifier {

    @Override
    public String name() {
        return "add";
    }

    @Override
    public BigDecimal apply(BigDecimal left, BigDecimal right) {
        return left.add(right);
    }
}
