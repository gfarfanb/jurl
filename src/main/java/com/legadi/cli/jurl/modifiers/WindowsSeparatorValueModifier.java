package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.annotations.Evaluable.Operation.STARTS_WITH;

import java.util.function.Function;

import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "win_separator")
@Evaluable(values = { "win_separator" }, op = STARTS_WITH)
public class WindowsSeparatorValueModifier implements ValueModifier {

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String apply(Function<String, String> propertyResolver, String[] args, String value) throws Exception {
        return value.replaceAll("\\\\", "\\\\\\\\");
    }
}
