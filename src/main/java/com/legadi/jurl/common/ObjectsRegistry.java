package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.LoaderUtils.instantiate;
import static com.legadi.jurl.common.LoaderUtils.typeOf;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.legadi.jurl.assertions.AssertionFunction;
import com.legadi.jurl.assertions.ContainsAssertionFunction;
import com.legadi.jurl.assertions.DoesNotContainsAssertionFunction;
import com.legadi.jurl.assertions.DoesNotMatchAssertionFunction;
import com.legadi.jurl.assertions.EndsWithAssertionFunction;
import com.legadi.jurl.assertions.EqualsToAssertionFunction;
import com.legadi.jurl.assertions.GreaterThanAssertionFunction;
import com.legadi.jurl.assertions.GreaterThanOrEqualsToAssertionFunction;
import com.legadi.jurl.assertions.IsBetweenAssertionFunction;
import com.legadi.jurl.assertions.IsBlankAssertionFunction;
import com.legadi.jurl.assertions.IsFalseAssertionFunction;
import com.legadi.jurl.assertions.IsInAssertionFunction;
import com.legadi.jurl.assertions.IsNotBetweenAssertionFunction;
import com.legadi.jurl.assertions.IsNotBlankAssertionFunction;
import com.legadi.jurl.assertions.IsNotInAssertionFunction;
import com.legadi.jurl.assertions.IsNotNullAssertionFunction;
import com.legadi.jurl.assertions.IsNullAssertionFunction;
import com.legadi.jurl.assertions.IsTrueAssertionFunction;
import com.legadi.jurl.assertions.LessThanAssertionFunction;
import com.legadi.jurl.assertions.LessThanOrEqualsToAssertionFunction;
import com.legadi.jurl.assertions.MatchesAssertionFunction;
import com.legadi.jurl.assertions.NotEqualsToAssertionFunction;
import com.legadi.jurl.assertions.StartsWithAssertionFunction;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.executor.RequestExecutor;
import com.legadi.jurl.executor.RequestModifier;
import com.legadi.jurl.executor.ResponseProcessor;
import com.legadi.jurl.executor.decoder.GzipOutputDecoder;
import com.legadi.jurl.executor.decoder.OutputDecoder;
import com.legadi.jurl.executor.http.HTTPRequestExecutor;
import com.legadi.jurl.executor.http.HTTPRequestModifier;
import com.legadi.jurl.executor.http.HTTPResponseProcessor;
import com.legadi.jurl.executor.mixer.BodyMixer;
import com.legadi.jurl.executor.mixer.JsonBodyMixer;
import com.legadi.jurl.executor.reader.JsonOutputReader;
import com.legadi.jurl.executor.reader.OutputReader;
import com.legadi.jurl.generators.AlphaNumericGenerator;
import com.legadi.jurl.generators.BooleanGenerator;
import com.legadi.jurl.generators.DateTimeGenerator;
import com.legadi.jurl.generators.DecimalGenerator;
import com.legadi.jurl.generators.EmailGenerator;
import com.legadi.jurl.generators.FullNameGenerator;
import com.legadi.jurl.generators.Generator;
import com.legadi.jurl.generators.IntegerGenerator;
import com.legadi.jurl.generators.LastNameGenerator;
import com.legadi.jurl.generators.LoremIpsumGenerator;
import com.legadi.jurl.generators.NameGenerator;
import com.legadi.jurl.generators.PasswordInputGenerator;
import com.legadi.jurl.generators.PickAnyGenerator;
import com.legadi.jurl.generators.UUIDGenerator;
import com.legadi.jurl.generators.UserInputGenerator;
import com.legadi.jurl.modifiers.DateTimeValueModifier;
import com.legadi.jurl.modifiers.DefaultValueModifier;
import com.legadi.jurl.modifiers.NumberValueModifier;
import com.legadi.jurl.modifiers.StringValueModifier;
import com.legadi.jurl.modifiers.ValueModifier;
import com.legadi.jurl.modifiers.WindowsSeparatorValueModifier;
import com.legadi.jurl.options.CleanOutputOption;
import com.legadi.jurl.options.CurlPrintOption;
import com.legadi.jurl.options.CustomClassOption;
import com.legadi.jurl.options.EnvironmentCopyOption;
import com.legadi.jurl.options.EnvironmentOption;
import com.legadi.jurl.options.EnvironmentRemoveValueOption;
import com.legadi.jurl.options.EnvironmentSetValueOption;
import com.legadi.jurl.options.HelpOption;
import com.legadi.jurl.options.MergeBodyOption;
import com.legadi.jurl.options.MockRequestOption;
import com.legadi.jurl.options.OpenEditorOption;
import com.legadi.jurl.options.Option;
import com.legadi.jurl.options.OverrideRequestOption;
import com.legadi.jurl.options.RequestPrintOption;
import com.legadi.jurl.options.RequestTypeOption;
import com.legadi.jurl.options.SetInputNameOption;
import com.legadi.jurl.options.SetValueOption;
import com.legadi.jurl.options.SkipAssertionsOption;
import com.legadi.jurl.options.SkipAuthenticationOption;
import com.legadi.jurl.options.SkipConditionsOption;
import com.legadi.jurl.options.TimesRepeatOption;
import com.legadi.jurl.parser.HTTPRequestParser;
import com.legadi.jurl.parser.RequestParser;

public class ObjectsRegistry {

    private static final Map<Class<?>, List<Pair<Evaluable, Spec>>> EVALUABLES = new HashMap<>();
    private static final Map<Class<?>, Map<String, Spec>> NAMED = new HashMap<>();

    private static final List<Pair<Evaluable, Spec>> EMTPY_EVALUABLES = new LinkedList<>();
    private static final Map<String, Spec> EMTPY_NAMED = new HashMap<>();

    private static final Set<Class<?>> GROUP_CLASSES = new HashSet<>();
    private static final Map<Class<?>, Spec> REGISTERED_CLASSES = new HashMap<>();

    private static final Map<String, Spec> CLASS_NAMES = new HashMap<>();

    static {
        register(Option.class, CleanOutputOption.class);
        register(Option.class, CurlPrintOption.class);
        register(Option.class, CustomClassOption.class);
        register(Option.class, EnvironmentCopyOption.class);
        register(Option.class, EnvironmentOption.class);
        register(Option.class, EnvironmentRemoveValueOption.class);
        register(Option.class, EnvironmentSetValueOption.class);
        register(Option.class, HelpOption.class);
        register(Option.class, MergeBodyOption.class);
        register(Option.class, MockRequestOption.class);
        register(Option.class, OpenEditorOption.class);
        register(Option.class, OverrideRequestOption.class);
        register(Option.class, RequestPrintOption.class);
        register(Option.class, RequestTypeOption.class);
        register(Option.class, SetInputNameOption.class);
        register(Option.class, SetValueOption.class);
        register(Option.class, SkipAssertionsOption.class);
        register(Option.class, SkipAuthenticationOption.class);
        register(Option.class, SkipConditionsOption.class);
        register(Option.class, TimesRepeatOption.class);

        register(AssertionFunction.class, ContainsAssertionFunction.class);
        register(AssertionFunction.class, DoesNotContainsAssertionFunction.class);
        register(AssertionFunction.class, DoesNotMatchAssertionFunction.class);
        register(AssertionFunction.class, EndsWithAssertionFunction.class);
        register(AssertionFunction.class, EqualsToAssertionFunction.class);
        register(AssertionFunction.class, GreaterThanAssertionFunction.class);
        register(AssertionFunction.class, GreaterThanOrEqualsToAssertionFunction.class);
        register(AssertionFunction.class, IsBetweenAssertionFunction.class);
        register(AssertionFunction.class, IsBlankAssertionFunction.class);
        register(AssertionFunction.class, IsFalseAssertionFunction.class);
        register(AssertionFunction.class, IsInAssertionFunction.class);
        register(AssertionFunction.class, IsNotBetweenAssertionFunction.class);
        register(AssertionFunction.class, IsNotBlankAssertionFunction.class);
        register(AssertionFunction.class, IsNotInAssertionFunction.class);
        register(AssertionFunction.class, IsNotNullAssertionFunction.class);
        register(AssertionFunction.class, IsNullAssertionFunction.class);
        register(AssertionFunction.class, IsTrueAssertionFunction.class);
        register(AssertionFunction.class, LessThanAssertionFunction.class);
        register(AssertionFunction.class, LessThanOrEqualsToAssertionFunction.class);
        register(AssertionFunction.class, MatchesAssertionFunction.class);
        register(AssertionFunction.class, NotEqualsToAssertionFunction.class);
        register(AssertionFunction.class, StartsWithAssertionFunction.class);

        register(RequestParser.class, HTTPRequestParser.class);
        register(RequestModifier.class, HTTPRequestModifier.class);
        register(RequestExecutor.class, HTTPRequestExecutor.class);
        register(ResponseProcessor.class, HTTPResponseProcessor.class);

        register(OutputReader.class, JsonOutputReader.class);
        register(OutputDecoder.class, GzipOutputDecoder.class);

        register(Generator.class, AlphaNumericGenerator.class);
        register(Generator.class, BooleanGenerator.class);
        register(Generator.class, DateTimeGenerator.class);
        register(Generator.class, DecimalGenerator.class);
        register(Generator.class, EmailGenerator.class);
        register(Generator.class, FullNameGenerator.class);
        register(Generator.class, IntegerGenerator.class);
        register(Generator.class, LastNameGenerator.class);
        register(Generator.class, LoremIpsumGenerator.class);
        register(Generator.class, NameGenerator.class);
        register(Generator.class, PasswordInputGenerator.class);
        register(Generator.class, PickAnyGenerator.class);
        register(Generator.class, UserInputGenerator.class);
        register(Generator.class, UUIDGenerator.class);

        register(ValueModifier.class, DateTimeValueModifier.class);
        register(ValueModifier.class, DefaultValueModifier.class);
        register(ValueModifier.class, NumberValueModifier.class);
        register(ValueModifier.class, StringValueModifier.class);
        register(ValueModifier.class, WindowsSeparatorValueModifier.class);

        register(BodyMixer.class, JsonBodyMixer.class);
    }

    private ObjectsRegistry() {}

    public static <T> T register(Class<?> groupClass, String typeClass, Object... args) {
        if(CLASS_NAMES.containsKey(typeClass)) {
            Spec entry = CLASS_NAMES.get(typeClass);
            return instantiate(entry.getTypeClass(), entry.getArgs());
        } else {
            T instance = register(groupClass, typeOf(typeClass), args);
            CLASS_NAMES.put(typeClass, new Spec(typeOf(typeClass), args));
            return instance;
        }
    }

    @SuppressWarnings("unchecked")
    public static <T> T register(Class<?> groupClass, Class<?> typeClass, Object... args) {
        boolean wasRegistered = false;
        Object result = null;

        if(isSubClassOf(Evaluable.class, typeClass)) {
            result = registerEvaluable(groupClass, typeClass, args);
            wasRegistered = true;
        }

        if(isSubClassOf(Named.class, typeClass)) {
            result = registerNamed(groupClass, typeClass, args);
            wasRegistered = true;
        }

        if(!wasRegistered) {
            throw new IllegalStateException("Type was not registered: " + typeClass);
        } else {
            GROUP_CLASSES.add(groupClass);
            REGISTERED_CLASSES.put(typeClass, new Spec(typeClass, args));
        }

        return (T) result;
    }

    public static boolean containsName(Class<? extends Named> groupClass, String name) {
        Spec entry = queryLastNamed(groupClass, name);
        return entry != null;
    }

    public static <T extends Named> Optional<T> findByName(Class<? extends Named> typeClass, String name) {
        return Optional.ofNullable(queryLastNamed(typeClass, name))
            .map(spec -> instantiate(spec.getTypeClass(), spec.getArgs()));
    }

    public static <T extends Named> T findByNameOrFail(Class<? extends Named> typeClass, String name) {
        Spec entry = queryLastNamed(typeClass, name);
        return validateAndInstantiate(entry, typeClass, name);
    }

    public static <T extends Evaluable> Optional<T> find(Class<? extends Evaluable> typeClass, String input) {
        return Optional.ofNullable(queryLastEvaluable(typeClass, input))
            .map(spec -> instantiate(spec.getTypeClass(), spec.getArgs()));
    }

    public static <T extends Evaluable> T findOrFail(Class<? extends Evaluable> typeClass, String input) {
        Spec entry = queryLastEvaluable(typeClass, input);
        return validateAndInstantiate(entry, typeClass, input);
    }

    public static <T> T findByType(Class<?> typeClass) {
        Spec entry = REGISTERED_CLASSES.get(typeClass);
        if(entry == null) {
            throw new IllegalStateException("Unable to find type: " + typeClass);
        }
        return instantiate(entry.getTypeClass(), entry.getArgs());
    }

    public static boolean isSubClassOf(Class<?> expectedClass, Class<?> typeClass) {
        try {
            typeClass.asSubclass(expectedClass);
            return true;
        } catch(ClassCastException ex) {
            return false;
        }
    }

    public static Set<Class<?>> getGroupClasses() {
        return GROUP_CLASSES;
    }

    @SuppressWarnings("unchecked")
    public static <T> List<T> getAllRegisteredByClassOf(Class<T> groupClass) {
        return entriesByGroup(groupClass)
            .stream()
            .filter(spec -> !CLASS_NAMES.containsKey(spec.getTypeClass().getName()))
            .map(spec -> instantiate(spec.getTypeClass(), spec.getArgs()))
            .map(object -> (T) object)
            .collect(Collectors.toList());
    }

    @SuppressWarnings("unchecked")
    public static <T> List<T> getAllRegisteredByNameOf(Class<T> groupClass) {
        return entriesByGroup(groupClass)
            .stream()
            .filter(spec -> CLASS_NAMES.containsKey(spec.getTypeClass().getName()))
            .map(spec -> instantiate(spec.getTypeClass(), spec.getArgs()))
            .map(object -> (T) object)
            .collect(Collectors.toList());
    }

    private static Object registerEvaluable(Class<?> groupClass, Class<?> typeClass, Object... args) {
        List<Pair<Evaluable, Spec>> entries = EVALUABLES.get(groupClass);

        if(entries == null) {
            entries = new LinkedList<>();
            EVALUABLES.put(groupClass, entries);
        }

        Evaluable evaluable = instantiate(typeClass, args);

        entries.add(new Pair<>(evaluable, new Spec(typeClass, args)));

        return evaluable;
    }

    private static Object registerNamed(Class<?> groupClass, Class<?> typeClass, Object... args) {
        Map<String, Spec> entries = NAMED.get(groupClass);

        if(entries == null) {
            entries = new HashMap<>();
            NAMED.put(groupClass, entries);
        }

        Named named = instantiate(typeClass, args);

        if(!named.allowOverride()) {
            if(entries.containsKey(named.name().toLowerCase())) {
                throw new IllegalStateException("Type with name '" + named.name() + "' was already registered: " + typeClass);
            }
            if(isNotBlank(named.alias()) && entries.containsKey(named.alias().toLowerCase())) {
                throw new IllegalStateException("Type with alias '" + named.alias() + "' was already registered: " + typeClass);
            }
        }

        entries.put(named.name().toLowerCase(), new Spec(typeClass, args));

        if(isNotBlank(named.alias())) {
            entries.put(named.alias().toLowerCase(), new Spec(typeClass, args));
        }

        return named;
    }

    private static <T> T validateAndInstantiate(Spec entry, Class<?> typeClass, String input) {
        if(entry == null) {
            throw new CommandException("Unable to obtain a registered class for:" + typeClass + ":" + input);
        }

        return instantiate(entry.getTypeClass(), entry.getArgs());
    }

    private static <T extends Evaluable> Spec queryLastEvaluable(Class<T> typeClass, String input) {
        List<Spec> result = EVALUABLES.getOrDefault(typeClass, EMTPY_EVALUABLES)
            .stream()
            .filter(p -> p.getLeft().accepts(input))
            .map(Pair::getRight)
            .collect(Collectors.toCollection(ArrayList::new));

        if(result.isEmpty()) {
            return null;
        }

        return result.get(result.size() - 1);
    }

    private static <T extends Named> Spec queryLastNamed(Class<T> typeClass, String name) {
        if(isBlank(name)) {
            return null;
        }
        return NAMED.getOrDefault(typeClass, EMTPY_NAMED).get(name.toLowerCase());
    }

    private static Set<Spec> entriesByGroup(Class<?> groupClass) {
        Set<Spec> entries = new HashSet<>();
        entries.addAll(EVALUABLES.getOrDefault(groupClass, EMTPY_EVALUABLES)
            .stream()
            .map(Pair::getRight)
            .collect(Collectors.toSet())
        );
        entries.addAll(NAMED.getOrDefault(groupClass, EMTPY_NAMED)
            .values()
            .stream()
            .collect(Collectors.toSet())
        );
        return entries;
    }

    public static class Spec {

        private final Class<?> typeClass;
        private final Object[] args;

        public Spec(Class<?> typeClass, Object[] args) {
            this.typeClass = typeClass;
            this.args = args;
        }

        public Class<?> getTypeClass() {
            return typeClass;
        }

        public Object[] getArgs() {
            return args;
        }

        @Override
        public int hashCode() {
            return Objects.hash(typeClass);
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            Spec other = (Spec) obj;
            return Objects.equals(typeClass, other.typeClass);
        }
    }
}
