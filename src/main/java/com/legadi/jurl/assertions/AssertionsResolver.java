package com.legadi.jurl.assertions;

import static com.legadi.jurl.assertions.AssertionsRegistry.findByName;
import static com.legadi.jurl.assertions.AssertionsRegistry.registerAssertionFunction;
import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isEmpty;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Logger;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.common.StringExpander;
import com.legadi.jurl.exception.AssertionException;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.AssertionEntry;
import com.legadi.jurl.model.AssertionResult;

public class AssertionsResolver {

    private static final Logger LOGGER = Logger.getLogger(AssertionsResolver.class.getName());

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

                if(assertionEntry.getType() != null) {
                    function = findByName(assertionEntry.getType().name());
                } else if(isBlank(assertionEntry.getAssertionClass())) {
                    throw new CommandException("Assertion class is null or empty, please specify a 'type' or an 'assertionClass'");
                } else {
                    String assertionClass = stringExpander.replaceAllInContent(values,
                        assertionEntry.getAssertionClass());
                    function = registerAssertionFunction(assertionClass);
                }

                if(isNotBlank(assertionEntry.getMessage())) {
                    message = stringExpander.replaceAllInContent(values, assertionEntry.getMessage());
                }
                
                String[] args = Arrays.stream(assertionEntry.getArgs())
                    .map(arg -> stringExpander.replaceAllInContent(values, arg))
                    .toArray(String[]::new);

                function.evaluate(message, args);
            } catch(AssertionException ex) {
                LOGGER.warning(ex.getMessage());
                result.setPassed(false);
                failures++;
            }
        }

        result.setFailures(failures);
        return Optional.of(result);
    }
}
