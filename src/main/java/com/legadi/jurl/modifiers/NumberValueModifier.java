package com.legadi.jurl.modifiers;

import java.math.BigDecimal;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.InvalidModifierOperationException;

public class NumberValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "Number";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "(add|subtract|multiply|divide)", "input" };
    }

    @Override
    public String apply(Settings settings, String[] args, String value) throws Exception {
        BigDecimal number = new BigDecimal(value);
        BigDecimal input = new BigDecimal(settings.getOrDefault(args[1], args[1]));

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
