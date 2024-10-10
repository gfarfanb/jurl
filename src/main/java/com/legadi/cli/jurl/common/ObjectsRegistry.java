package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.LoaderUtils.instantiate;
import static com.legadi.cli.jurl.common.LoaderUtils.typeOf;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.legadi.cli.jurl.assertions.AssertionFunction;
import com.legadi.cli.jurl.assertions.ContainsAssertionFunction;
import com.legadi.cli.jurl.assertions.DoesNotContainsAssertionFunction;
import com.legadi.cli.jurl.assertions.DoesNotMatchAssertionFunction;
import com.legadi.cli.jurl.assertions.EndsWithAssertionFunction;
import com.legadi.cli.jurl.assertions.EqualsToAssertionFunction;
import com.legadi.cli.jurl.assertions.GreaterThanAssertionFunction;
import com.legadi.cli.jurl.assertions.GreaterThanOrEqualsToAssertionFunction;
import com.legadi.cli.jurl.assertions.IsBetweenAssertionFunction;
import com.legadi.cli.jurl.assertions.IsBlankAssertionFunction;
import com.legadi.cli.jurl.assertions.IsFalseAssertionFunction;
import com.legadi.cli.jurl.assertions.IsInAssertionFunction;
import com.legadi.cli.jurl.assertions.IsNotBetweenAssertionFunction;
import com.legadi.cli.jurl.assertions.IsNotBlankAssertionFunction;
import com.legadi.cli.jurl.assertions.IsNotInAssertionFunction;
import com.legadi.cli.jurl.assertions.IsNotNullAssertionFunction;
import com.legadi.cli.jurl.assertions.IsNullAssertionFunction;
import com.legadi.cli.jurl.assertions.IsTrueAssertionFunction;
import com.legadi.cli.jurl.assertions.LessThanAssertionFunction;
import com.legadi.cli.jurl.assertions.LessThanOrEqualsToAssertionFunction;
import com.legadi.cli.jurl.assertions.MatchesAssertionFunction;
import com.legadi.cli.jurl.assertions.NotEqualsToAssertionFunction;
import com.legadi.cli.jurl.assertions.StartsWithAssertionFunction;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.executor.RequestExecutor;
import com.legadi.cli.jurl.executor.RequestModifier;
import com.legadi.cli.jurl.executor.ResponseProcessor;
import com.legadi.cli.jurl.executor.decoder.GzipOutputDecoder;
import com.legadi.cli.jurl.executor.decoder.OutputDecoder;
import com.legadi.cli.jurl.executor.http.HTTPRequestExecutor;
import com.legadi.cli.jurl.executor.http.HTTPRequestModifier;
import com.legadi.cli.jurl.executor.http.HTTPResponseProcessor;
import com.legadi.cli.jurl.executor.mixer.BodyMixer;
import com.legadi.cli.jurl.executor.mixer.JsonBodyMixer;
import com.legadi.cli.jurl.executor.reader.JsonOutputReader;
import com.legadi.cli.jurl.executor.reader.OutputReader;
import com.legadi.cli.jurl.generators.AlphaNumericGenerator;
import com.legadi.cli.jurl.generators.BooleanGenerator;
import com.legadi.cli.jurl.generators.DateTimeGenerator;
import com.legadi.cli.jurl.generators.DecimalGenerator;
import com.legadi.cli.jurl.generators.EmailGenerator;
import com.legadi.cli.jurl.generators.FullNameGenerator;
import com.legadi.cli.jurl.generators.Generator;
import com.legadi.cli.jurl.generators.IntegerGenerator;
import com.legadi.cli.jurl.generators.LastNameGenerator;
import com.legadi.cli.jurl.generators.LoremIpsumGenerator;
import com.legadi.cli.jurl.generators.NameGenerator;
import com.legadi.cli.jurl.generators.PasswordInputGenerator;
import com.legadi.cli.jurl.generators.PickAnyGenerator;
import com.legadi.cli.jurl.generators.UUIDGenerator;
import com.legadi.cli.jurl.generators.UserInputGenerator;
import com.legadi.cli.jurl.modifiers.AddValueModifier;
import com.legadi.cli.jurl.modifiers.DateEpochValueModifier;
import com.legadi.cli.jurl.modifiers.DateMinusValueModifier;
import com.legadi.cli.jurl.modifiers.DatePlusValueModifier;
import com.legadi.cli.jurl.modifiers.DefaultValueModifier;
import com.legadi.cli.jurl.modifiers.DivideValueModifier;
import com.legadi.cli.jurl.modifiers.MultiplyValueModifier;
import com.legadi.cli.jurl.modifiers.PrefixValueModifier;
import com.legadi.cli.jurl.modifiers.ReadFileModifier;
import com.legadi.cli.jurl.modifiers.SubtractValueModifier;
import com.legadi.cli.jurl.modifiers.SuffixValueModifier;
import com.legadi.cli.jurl.modifiers.ValueModifier;
import com.legadi.cli.jurl.modifiers.WindowsSeparatorValueModifier;
import com.legadi.cli.jurl.options.CleanOutputOption;
import com.legadi.cli.jurl.options.CurlPrintOption;
import com.legadi.cli.jurl.options.CustomClassOption;
import com.legadi.cli.jurl.options.DownloadInOption;
import com.legadi.cli.jurl.options.EnvironmentCopyOption;
import com.legadi.cli.jurl.options.EnvironmentOption;
import com.legadi.cli.jurl.options.EnvironmentRemoveValueOption;
import com.legadi.cli.jurl.options.EnvironmentSetValueOption;
import com.legadi.cli.jurl.options.HelpOption;
import com.legadi.cli.jurl.options.MergeBodyOption;
import com.legadi.cli.jurl.options.MockRequestOption;
import com.legadi.cli.jurl.options.OpenEditorOption;
import com.legadi.cli.jurl.options.Option;
import com.legadi.cli.jurl.options.OverrideRequestOption;
import com.legadi.cli.jurl.options.RequestPrintOption;
import com.legadi.cli.jurl.options.RequestTypeOption;
import com.legadi.cli.jurl.options.SetInputNameOption;
import com.legadi.cli.jurl.options.SetValueOption;
import com.legadi.cli.jurl.options.SkipAssertionsOption;
import com.legadi.cli.jurl.options.SkipAuthenticationOption;
import com.legadi.cli.jurl.options.SkipConditionsOption;
import com.legadi.cli.jurl.options.SkipUserInputOption;
import com.legadi.cli.jurl.options.StartInStepOption;
import com.legadi.cli.jurl.options.TimesRepeatOption;
import com.legadi.cli.jurl.parser.HTTPRequestParser;
import com.legadi.cli.jurl.parser.RequestParser;

public class ObjectsRegistry {

    private static final Map<Class<?>, List<Pair<Evaluable, Spec>>> EVALUABLES = new HashMap<>();
    private static final Map<Class<?>, Map<String, Spec>> NAMED = new HashMap<>();

    private static final List<Pair<Evaluable, Spec>> EMTPY_EVALUABLES = new ArrayList<>();
    private static final Map<String, Spec> EMTPY_NAMED = new HashMap<>();

    private static final Set<Class<?>> GROUP_CLASSES = new HashSet<>();
    private static final Map<Class<?>, Spec> REGISTERED_CLASSES = new HashMap<>();

    private static final Map<String, Spec> CLASS_NAMES = new HashMap<>();

    static {
        register(Option.class, CleanOutputOption.class);
        register(Option.class, CurlPrintOption.class);
        register(Option.class, CustomClassOption.class);
        register(Option.class, DownloadInOption.class);
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
        register(Option.class, SkipUserInputOption.class);
        register(Option.class, StartInStepOption.class);
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

        register(ValueModifier.class, AddValueModifier.class);
        register(ValueModifier.class, DateEpochValueModifier.class);
        register(ValueModifier.class, DateMinusValueModifier.class);
        register(ValueModifier.class, DatePlusValueModifier.class);
        register(ValueModifier.class, DefaultValueModifier.class);
        register(ValueModifier.class, DivideValueModifier.class);
        register(ValueModifier.class, MultiplyValueModifier.class);
        register(ValueModifier.class, PrefixValueModifier.class);
        register(ValueModifier.class, ReadFileModifier.class);
        register(ValueModifier.class, SubtractValueModifier.class);
        register(ValueModifier.class, SuffixValueModifier.class);
        register(ValueModifier.class, WindowsSeparatorValueModifier.class);

        register(BodyMixer.class, JsonBodyMixer.class);
    }

    private ObjectsRegistry() {}

    public static <T> T register(Class<?> groupClass, String typeClass, Object... args) {
        if(CLASS_NAMES.get(typeClass) != null) {
            Spec entry = CLASS_NAMES.get(typeClass);
            return instantiate(entry.getTypeClass(), entry.getArgs());
        } else {
            Class<?> type = typeOf(typeClass);
            T instance = register(groupClass, type, args);
            CLASS_NAMES.put(typeClass, new Spec(type, args));
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
            .filter(spec -> CLASS_NAMES.get(spec.getTypeClass().getName()) == null)
            .map(spec -> instantiate(spec.getTypeClass(), spec.getArgs()))
            .map(object -> (T) object)
            .collect(Collectors.toList());
    }

    @SuppressWarnings("unchecked")
    public static <T> List<T> getAllRegisteredByNameOf(Class<T> groupClass) {
        return entriesByGroup(groupClass)
            .stream()
            .filter(spec -> CLASS_NAMES.get(spec.getTypeClass().getName()) != null)
            .map(spec -> instantiate(spec.getTypeClass(), spec.getArgs()))
            .map(object -> (T) object)
            .collect(Collectors.toList());
    }

    private static Object registerEvaluable(Class<?> groupClass, Class<?> typeClass, Object... args) {
        List<Pair<Evaluable, Spec>> entries = EVALUABLES.get(groupClass);

        if(entries == null) {
            entries = new ArrayList<>();
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
            if(entries.get(named.name().toLowerCase()) != null) {
                throw new IllegalStateException("Type with name '" + named.name() + "' was already registered: " + typeClass);
            }
            if(isNotBlank(named.alias()) && entries.get(named.alias().toLowerCase()) != null) {
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
