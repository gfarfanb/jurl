package com.legadi.jurl.common;

import static com.legadi.jurl.common.ObjectsRegistry.find;
import static com.legadi.jurl.common.ObjectsRegistry.findByName;
import static com.legadi.jurl.common.ObjectsRegistry.findOrFail;
import static com.legadi.jurl.common.ObjectsRegistry.register;

import java.util.Optional;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.CommandException;

public class ObjectsRegistryTest {

    @Test
    public void registerEvaluableByClassName() {
        register(Evaluable.class, TestEvaluable.class.getName());

        Evaluable evaluable = Assertions.assertDoesNotThrow(
            () -> findOrFail(Evaluable.class, "test"));

        Assertions.assertNotNull(evaluable);

        Optional<Evaluable> evaluableOptional = Assertions.assertDoesNotThrow(
            () -> find(Evaluable.class, "test"));

        Assertions.assertNotNull(evaluableOptional.isPresent());
    }

    @Test
    public void registerEvaluableByClass() {
        register(Evaluable.class, TestEvaluable.class);

        Evaluable evaluable = Assertions.assertDoesNotThrow(
            () -> findOrFail(Evaluable.class, "test"));

        Assertions.assertNotNull(evaluable);

        Optional<Evaluable> evaluableOptional = Assertions.assertDoesNotThrow(
            () -> find(Evaluable.class, "test"));

        Assertions.assertNotNull(evaluableOptional.isPresent());
    }

    @Test
    public void registerNamedByClass() {
        register(Named.class, TestNamed.class);

        Optional<Named> named = Assertions.assertDoesNotThrow(
            () -> findByName(Named.class, "test-named-entry"));

        Assertions.assertTrue(named.isPresent());
    }

    @Test
    public void invalidType() {
        Assertions.assertThrows(IllegalStateException.class,
            () -> register(Registry.class, RegistryClass.class));
    }

    @Test
    public void namedDuplicate() {
        register(Named.class, TestDuplication.class);

        Assertions.assertThrows(IllegalStateException.class,
            () -> register(Named.class, TestDuplication.class));
    }

    @Test
    public void evaluableNotFound() {
        Assertions.assertThrows(CommandException.class,
            () -> findOrFail(NotRegistered.class, "not-found"));

        Optional<NotRegistered> notFound = Assertions.assertDoesNotThrow(
            () -> find(NotRegistered.class, "not-found"));

        Assertions.assertFalse(notFound.isPresent());
    }

    @Test
    public void namedNotFound() {
        Optional<Named> named = Assertions.assertDoesNotThrow(
            () -> findByName(Named.class, "not-found"));

        Assertions.assertFalse(named.isPresent());
    }

    public static interface Registry {

    }

    public static class RegistryClass implements Registry {

    }

    public static class NotRegistered implements Evaluable {

        @Override
        public boolean accepts(String input) {
            return true;
        }
    }

    public static class TestEvaluable implements Evaluable {

        @Override
        public boolean accepts(String input) {
            return "test".equals(input);
        }
    }

    public static class TestNamed implements Named {

        @Override
        public String name() {
            return "test-named-entry";
        }

        @Override
        public boolean allowOverride() {
            return false;
        }
    }

    public static class TestDuplication implements Named {

        @Override
        public String name() {
            return "test-duplication";
        }

        @Override
        public boolean allowOverride() {
            return false;
        }
    }
}
