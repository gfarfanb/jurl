package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isEmpty;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.ObjectsRegistry.containsName;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByTypeName;
import static com.legadi.cli.jurl.common.ObjectsRegistry.register;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.StringExpander;
import com.legadi.cli.jurl.exception.AssertionException;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.AssertionEntry;
import com.legadi.cli.jurl.model.AssertionResult;

public class AssertionsResolver {

    private AssertionsResolver() {}

    public static Optional<AssertionResult> evaluate(Settings settings,
            List<AssertionEntry> assertions) {
        return evaluate(settings, new HashMap<>(), assertions);
    }

    public static Optional<AssertionResult> evaluate(Settings settings,
            Map<String, String> values, List<AssertionEntry> assertions) {
        if(isEmpty(assertions)) {
            return Optional.empty();
        }

        StringExpander stringExpander = new StringExpander(settings);
        AssertionResult result = new AssertionResult(assertions.size());
        int failures = 0;

        for(AssertionEntry assertionEntry : assertions) {
            try {
                AssertionFunction function = null;
                String message = null;

                if(containsName(AssertionFunction.class, assertionEntry.getName())) {
                    function = findByNameOrFail(AssertionFunction.class, assertionEntry.getName());
                } else if(isBlank(assertionEntry.getAssertionClass())) {
                    throw new CommandException("Assertion class is null or empty, please specify a 'type' or an 'assertionClass'");
                } else {
                    String assertionClass = stringExpander.replaceAllInContent(values,
                        assertionEntry.getAssertionClass());
                    register(AssertionFunction.class, assertionClass);
                    function = findByTypeName(assertionClass);
                }

                if(isNotBlank(assertionEntry.getMessage())) {
                    message = stringExpander.replaceAllInContent(values, assertionEntry.getMessage());
                }
                
                String[] args = Arrays.stream(assertionEntry.getArgs())
                    .map(arg -> stringExpander.replaceAllInContent(values, arg))
                    .toArray(String[]::new);

                function.evaluate(assertionEntry, message, args);
            } catch(AssertionException ex) {
                result.getFailedMessages().add(ex.getMessage());
                result.setPassed(false);
                failures++;
            }
        }

        result.setFailures(failures);
        return Optional.of(result);
    }
}
