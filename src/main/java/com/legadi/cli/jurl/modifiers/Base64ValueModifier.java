package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.annotations.Evaluable.Operation.STARTS_WITH;

import java.util.Base64;
import java.util.function.Function;

import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;
import com.legadi.cli.jurl.exception.CommandException;

@Named(name = "base64")
@Evaluable(values = { "base64" }, op = STARTS_WITH)
public class Base64ValueModifier implements ValueModifier {

    @Override
    public String[] getArgs() {
        return new String[] { "operation" };
    }

    @Override
    public String apply(Function<String, String> propertyResolver, String[] args, String value) throws Exception {
        String input = propertyResolver.apply(value);
        String operation = args[0].toLowerCase();

        switch(operation) {
            case "encode":
                return new String(Base64.getEncoder().encode(input.getBytes()));
            case "decode":
                return new String(Base64.getDecoder().decode(input.getBytes()));
            default:
                throw new CommandException("Invalid Base 64 operation: " + operation);
        }
    }
}
