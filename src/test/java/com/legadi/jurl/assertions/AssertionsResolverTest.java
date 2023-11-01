package com.legadi.jurl.assertions;

import static com.legadi.jurl.assertions.AssertionsResolver.evaluate;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.AssertionException;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.AssertionEntry;
import com.legadi.jurl.model.AssertionResult;

public class AssertionsResolverTest {

    @Test
    public void evaluateValidation() {
        Settings settings = new Settings();
        List<AssertionEntry> entries = new LinkedList<>();
        entries.add(instance("EQUALS_TO", "test", "test"));
        entries.add(instance("==", "test", "test"));

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> evaluate(settings, entries));

        Assertions.assertTrue(result.isPresent());
        Assertions.assertTrue(result.get().isPassed());
        Assertions.assertEquals(entries.size(), result.get().getAssertions());
        Assertions.assertEquals(0, result.get().getFailures());
        Assertions.assertTrue(result.get().getFailedMessages().isEmpty());
    }

    @Test
    public void evaluateWithValues() {
        Settings settings = new Settings();
        List<AssertionEntry> entries = new LinkedList<>();
        entries.add(instance("EQUALS_TO", "{{param}}", "test"));
        entries.add(instance("==", "test", "{{param}}"));

        Map<String, String> values = new HashMap<>();
        values.put("param", "test");

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> evaluate(settings, values, entries));

        Assertions.assertTrue(result.isPresent());
        Assertions.assertTrue(result.get().isPassed());
        Assertions.assertEquals(entries.size(), result.get().getAssertions());
        Assertions.assertEquals(0, result.get().getFailures());
        Assertions.assertTrue(result.get().getFailedMessages().isEmpty());
    }

    @Test
    public void evaluateNotFound() {
        Settings settings = new Settings();
        List<AssertionEntry> entries = new LinkedList<>();
        entries.add(instance("NOT_FOUND", "test", "test"));

        Assertions.assertThrows(CommandException.class,
            () -> evaluate(settings, entries));
    }

    @Test
    public void evaluateWithMessage() {
        Settings settings = new Settings();
        List<AssertionEntry> entries = new LinkedList<>();
        AssertionEntry entry = instance("EQUALS_TO", "test", "test");

        entry.setMessage("Test");

        entries.add(entry);

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> evaluate(settings, entries));

        Assertions.assertTrue(result.isPresent());
        Assertions.assertTrue(result.get().isPassed());
        Assertions.assertEquals(entries.size(), result.get().getAssertions());
        Assertions.assertEquals(0, result.get().getFailures());
        Assertions.assertTrue(result.get().getFailedMessages().isEmpty());
    }

    @Test
    public void evaluateWithAssertionClass() {
        Settings settings = new Settings();
        List<AssertionEntry> entries = new LinkedList<>();
        AssertionEntry entry = instance(null);

        entry.setAssertionClass(ResolverAssertionFunction.class.getName());

        entries.add(entry);

        Optional<AssertionResult> result = Assertions.assertDoesNotThrow(
            () -> evaluate(settings, entries));

        Assertions.assertTrue(result.isPresent());
        Assertions.assertTrue(result.get().isPassed());
        Assertions.assertEquals(entries.size(), result.get().getAssertions());
        Assertions.assertEquals(0, result.get().getFailures());
        Assertions.assertTrue(result.get().getFailedMessages().isEmpty());
    }

    private AssertionEntry instance(String name, String... args) {
        AssertionEntry instance = new AssertionEntry();
        instance.setName(name);
        instance.setArgs(args);
        return instance;
    }

    public static class ResolverAssertionFunction implements AssertionFunction {

        @Override
        public String name() {
            return "resolver";
        }

        @Override
        public String[] getArgs() {
            return new String[0];
        }

        @Override
        public boolean apply(String[] args) throws AssertionException {
            return true;
        }
    }
}
