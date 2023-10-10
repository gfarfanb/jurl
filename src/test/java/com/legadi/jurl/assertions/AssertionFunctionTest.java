package com.legadi.jurl.assertions;

import static com.legadi.jurl.assertions.AssertionsRegistry.findByName;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;
import com.legadi.jurl.model.AssertionType;

public abstract class AssertionFunctionTest<T extends AssertionFunction> {

    private final T function;

    @SuppressWarnings("unchecked")
    public AssertionFunctionTest(AssertionType type) {
        this.function = (T) findByName(type.name());
    }

    @Test
    public void nullArguments() {
        String[] args = null;

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(args));
    }

    public void evaluate(String... args) {
        function.evaluate("Test: " + function.getClass().getName(), args);
    }
}
