package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedName;
import static com.legadi.cli.jurl.common.ObjectsRegistry.containsName;
import static com.legadi.cli.jurl.common.ObjectsRegistry.find;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByName;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByType;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findOrFail;
import static com.legadi.cli.jurl.common.ObjectsRegistry.getAllRegisteredByClassOf;
import static com.legadi.cli.jurl.common.ObjectsRegistry.getAllRegisteredByNameOf;
import static com.legadi.cli.jurl.common.ObjectsRegistry.getGroupClasses;
import static com.legadi.cli.jurl.common.ObjectsRegistry.register;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.assertions.AssertionFunction;
import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;
import com.legadi.cli.jurl.common.annotations.Typed;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.executor.RequestExecutor;
import com.legadi.cli.jurl.executor.RequestModifier;
import com.legadi.cli.jurl.executor.ResponseProcessor;
import com.legadi.cli.jurl.executor.decoder.OutputDecoder;
import com.legadi.cli.jurl.executor.mixer.BodyMixer;
import com.legadi.cli.jurl.executor.mixer.JsonBodyMixer;
import com.legadi.cli.jurl.executor.reader.OutputReader;
import com.legadi.cli.jurl.generators.AlphaNumericGenerator;
import com.legadi.cli.jurl.generators.Generator;
import com.legadi.cli.jurl.modifiers.ValueModifier;
import com.legadi.cli.jurl.options.Option;
import com.legadi.cli.jurl.parser.RequestParser;

public class ObjectsRegistryTest {

    @Test
    public void registerEvaluableByClassName() {
        register(TestEvaluable.class, TestEvaluable.class.getName());

        TestEvaluable evaluable = Assertions.assertDoesNotThrow(
            () -> findOrFail(TestEvaluable.class, "test"));

        Assertions.assertNotNull(evaluable);

        Optional<TestEvaluable> evaluableOptional = Assertions.assertDoesNotThrow(
            () -> find(TestEvaluable.class, "test"));

        Assertions.assertNotNull(evaluableOptional.isPresent());

        register(TestEvaluable.class, TestEvaluable.class.getName());
        TestEvaluable testEvaluable = findByNameOrFail(TestEvaluable.class, extractNamedName(evaluable));

        Assertions.assertNotNull(testEvaluable);
    }

    @Test
    public void registerEvaluableByClass() {
        register(TestEvaluable.class, TestEvaluable.class);

        TestEvaluable evaluable = Assertions.assertDoesNotThrow(
            () -> findOrFail(TestEvaluable.class, "test"));

        Assertions.assertNotNull(evaluable);

        Optional<Evaluable> evaluableOptional = Assertions.assertDoesNotThrow(
            () -> find(TestEvaluable.class, "test"));

        Assertions.assertNotNull(evaluableOptional.isPresent());
    }

    @Test
    public void registerNamedByClass() {
        register(Named.class, TestNamed.class);

        Assertions.assertTrue(containsName(Named.class, "test-named-entry"));

        Optional<Named> named = Assertions.assertDoesNotThrow(
            () -> findByName(Named.class, "test-named-entry"));

        Assertions.assertTrue(named.isPresent());

        TestNamed testNamed = Assertions.assertDoesNotThrow(
            () -> findByNameOrFail(Named.class, "test-named-entry"));

        Assertions.assertNotNull(testNamed);
    }

    @Test
    public void findByTypeValidation() {
        AlphaNumericGenerator generator = Assertions.assertDoesNotThrow(
            () -> findByType(AlphaNumericGenerator.class));

        Assertions.assertNotNull(generator);

        Assertions.assertThrows(IllegalStateException.class,
            () -> findByType(Registry.class));
    }

    @Test
    public void invalidType() {
        Assertions.assertThrows(IllegalStateException.class,
            () -> register(Registry.class, RegistryClass.class));
    }

    @Test
    public void namedDuplicate() {
        register(Named.class, TestNameDuplication.class);

        Assertions.assertThrows(IllegalStateException.class,
            () -> register(Named.class, TestNameDuplication.class));
    }

    @Test
    public void aliasDuplicate() {
        Assertions.assertThrows(IllegalStateException.class,
            () -> register(Option.class, TestAliasDuplication.class));
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
        Assertions.assertFalse(containsName(Named.class, "not-found"));
        Assertions.assertFalse(containsName(Named.class, null));

        Optional<Named> named = Assertions.assertDoesNotThrow(
            () -> findByName(Named.class, "not-found"));

        Assertions.assertFalse(named.isPresent());
    }

    @Test
    public void getGroupClassesValidation() {
        Class<?>[] expectedGroupClasses = {
            Option.class,
            AssertionFunction.class,
            RequestParser.class,
            RequestModifier.class,
            RequestExecutor.class,
            ResponseProcessor.class,
            OutputReader.class,
            OutputDecoder.class,
            Generator.class,
            ValueModifier.class,
            BodyMixer.class
        };
        Set<Class<?>> groupClasses = getGroupClasses();

        Assertions.assertTrue(Arrays.stream(expectedGroupClasses).allMatch(type -> groupClasses.contains(type)));
    }

    @Test
    public void getAllRegisteredByClassAndName() {
        Assertions.assertDoesNotThrow(
            () -> register(BodyMixer.class, TestBodyMixer.class.getName()));

        List<BodyMixer> mixersByClass = Assertions.assertDoesNotThrow(
            () -> getAllRegisteredByClassOf(BodyMixer.class));

        Assertions.assertTrue(mixersByClass.stream().anyMatch(instance -> instance.getClass() == JsonBodyMixer.class));
        Assertions.assertFalse(mixersByClass.stream().anyMatch(instance -> instance.getClass() == TestBodyMixer.class));

        List<BodyMixer> mixersByName = Assertions.assertDoesNotThrow(
            () -> getAllRegisteredByNameOf(BodyMixer.class));

        Assertions.assertTrue(mixersByName.stream().anyMatch(instance -> instance.getClass() == TestBodyMixer.class));
        Assertions.assertFalse(mixersByName.stream().anyMatch(instance -> instance.getClass() == JsonBodyMixer.class));
    }

    public static interface Registry {
    }

    public static class RegistryClass implements Registry {
    }

    @Evaluable(values = {}, op = Evaluable.Operation.ALWAYS_TRUE)
    public static class NotRegistered {

    }

    @Named(name = "test-evaluable", allowOverride = true)
    @Evaluable(values = { "test" }, op = Evaluable.Operation.EQUALS)
    public static class TestEvaluable {

    }

    @Named(name = "test-named-entry")
    public static class TestNamed {

    }

    @Named(name = "test-name-duplication")
    public static class TestNameDuplication{

    }

    @Named(name = "test-alias-duplication", alias = "-t")
    public static class TestAliasDuplication {

    }

    @Typed(type = "test")
    @Evaluable(values = { "body" }, op = Evaluable.Operation.EQUALS_IGNORE_CASE)
    public static class TestBodyMixer implements BodyMixer {

        @Override
        public Path apply(Settings settings, Map<String, Object> defaults, MixerEntry entry) {
            return null;
        }
    }
}
