package com.legadi.jurl.modifiers;

import java.math.BigDecimal;
import java.util.function.Function;

import com.legadi.jurl.exception.InvalidModifierOperationException;

public class NumberValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "number";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "(add|subtract|multiply|divide)", "input" };
    }

    @Override
    public String apply(Function<String, String> getter, String[] args, String value) throws Exception {
        BigDecimal number = new BigDecimal(value);
        BigDecimal input = new BigDecimal(getter.apply(args[1]));

        switch(args[0].toLowerCase()) {
            case "add":
                return number.add(input).toString();
            case "subtract":
                return number.subtract(input).toString();
            case "multiply":
                return number.multiply(input).toString();
            case "divide":
                return number.divide(input).toString();
            default:
                throw new InvalidModifierOperationException(args[0]);
        }
    }
}
