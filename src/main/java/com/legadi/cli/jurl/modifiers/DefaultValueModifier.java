package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.annotations.Evaluable.Operation.STARTS_WITH;

import java.util.function.Function;

import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "default")
@Evaluable(values = { "default" }, op = STARTS_WITH)
public class DefaultValueModifier implements ValueModifier {

    @Override
    public String[] getArgs() {
        return new String[] { "default-value" };
    }

    @Override
    public String apply(Function<String, String> propertyResolver, String[] args, String value) throws Exception {
        if(isBlank(value)) {
            return args[0];
        } else {
            return value;
        }
    }
}
