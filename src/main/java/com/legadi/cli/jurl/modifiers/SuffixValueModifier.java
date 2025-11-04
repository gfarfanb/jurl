package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.annotations.Evaluable.Operation.STARTS_WITH;

import java.util.function.Function;

import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "suffix")
@Evaluable(values = { "suffix" }, op = STARTS_WITH)
public class SuffixValueModifier implements ValueModifier {

    @Override
    public String[] getArgs() {
        return new String[] { "input" };
    }

    @Override
    public String apply(Function<String, String> propertyResolver, String[] args, String value) throws Exception {
        String input = propertyResolver.apply(args[0]);
        return value + input;
    }
}
