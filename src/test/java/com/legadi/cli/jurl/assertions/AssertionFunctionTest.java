package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.AssertionException;
import com.legadi.cli.jurl.model.AssertionEntry;
import com.legadi.cli.jurl.model.AssertionType;

public abstract class AssertionFunctionTest<T extends AssertionFunction> {

    private final AssertionEntry assertionEntry;
    private final T function;

    @SuppressWarnings("unchecked")
    public AssertionFunctionTest(String name) {
        this.function = (T) findByNameOrFail(AssertionFunction.class, name);
        this.assertionEntry = new AssertionEntry();
        assertionEntry.setName(function.name());
        assertionEntry.setType(AssertionType.ASSERTION);
    }

    @Test
    public void nullArguments() {
        String[] args = null;

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(args));
    }

    public void evaluate(String... args) {
        function.evaluate(assertionEntry, "Test: " + function.getClass().getName(), args);
    }
}
