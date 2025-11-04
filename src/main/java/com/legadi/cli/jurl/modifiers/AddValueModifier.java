package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.annotations.Evaluable.Operation.STARTS_WITH;

import java.math.BigDecimal;

import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "add")
@Evaluable(values = { "add" }, op = STARTS_WITH)
public class AddValueModifier extends NumberValueModifier {

    @Override
    public BigDecimal apply(BigDecimal left, BigDecimal right) {
        return left.add(right);
    }
}
