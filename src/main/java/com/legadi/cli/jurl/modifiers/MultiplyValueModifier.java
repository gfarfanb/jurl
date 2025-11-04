package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.annotations.Evaluable.Operation.STARTS_WITH;

import java.math.BigDecimal;

import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "multiply")
@Evaluable(values = { "multiply" }, op = STARTS_WITH)
public class MultiplyValueModifier extends NumberValueModifier {

    @Override
    public BigDecimal apply(BigDecimal left, BigDecimal right) {
        return left.multiply(right);
    }
}
