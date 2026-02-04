package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.cli.jurl.common.LoaderUtils.instantiate;
import static com.legadi.cli.jurl.common.LoaderUtils.typeOf;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
import com.legadi.cli.jurl.common.annotations.ConfigReplaceable;
import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.executor.HeaderAuthenticator;
import com.legadi.cli.jurl.executor.RequestExecutor;
import com.legadi.cli.jurl.executor.RequestModifier;
import com.legadi.cli.jurl.executor.ResponseProcessor;
import com.legadi.cli.jurl.executor.decoder.GzipOutputDecoder;
import com.legadi.cli.jurl.executor.decoder.OutputDecoder;
import com.legadi.cli.jurl.executor.http.HTTPBasicHeaderAuthenticator;
import com.legadi.cli.jurl.executor.http.HTTPRequestExecutor;
import com.legadi.cli.jurl.executor.http.HTTPRequestModifier;
import com.legadi.cli.jurl.executor.http.HTTPResponseProcessor;
import com.legadi.cli.jurl.executor.http.HTTPTokenHeaderAuthenticator;
import com.legadi.cli.jurl.executor.mixer.BodyMixer;
import com.legadi.cli.jurl.executor.mixer.JsonBodyMixer;
import com.legadi.cli.jurl.executor.reader.JsonOutputReader;
import com.legadi.cli.jurl.executor.reader.OutputReader;
import com.legadi.cli.jurl.generators.AlphaNumericGenerator;
import com.legadi.cli.jurl.generators.BooleanGenerator;
import com.legadi.cli.jurl.generators.DateTimeGenerator;
import com.legadi.cli.jurl.generators.DecimalGenerator;
import com.legadi.cli.jurl.generators.EmailGenerator;
import com.legadi.cli.jurl.generators.EmptyGenerator;
import com.legadi.cli.jurl.generators.EscapedGenerator;
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
import com.legadi.cli.jurl.modifiers.Base64ValueModifier;
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
import com.legadi.cli.jurl.options.ExecuteAuthenticationOption;
import com.legadi.cli.jurl.options.GetValueOption;
import com.legadi.cli.jurl.options.HelpOption;
import com.legadi.cli.jurl.options.ListRequestsOption;
import com.legadi.cli.jurl.options.MergeBodyOption;
import com.legadi.cli.jurl.options.MockRequestOption;
import com.legadi.cli.jurl.options.OpenEditorOption;
import com.legadi.cli.jurl.options.Option;
import com.legadi.cli.jurl.options.OverrideRequestOption;
import com.legadi.cli.jurl.options.RequestPrintOption;
import com.legadi.cli.jurl.options.RequestTypeOption;
import com.legadi.cli.jurl.options.SetFilterNameOption;
import com.legadi.cli.jurl.options.SetInputNameOption;
import com.legadi.cli.jurl.options.SetValueOption;
import com.legadi.cli.jurl.options.SkipAssertionsOption;
import com.legadi.cli.jurl.options.SkipAuthenticationOption;
import com.legadi.cli.jurl.options.SkipConditionsOption;
import com.legadi.cli.jurl.options.SkipUserInputOption;
import com.legadi.cli.jurl.options.StartInStepOption;
import com.legadi.cli.jurl.options.TimesRepeatOption;
import com.legadi.cli.jurl.options.UseAuthenticationOption;
import com.legadi.cli.jurl.parser.HTTPRequestParser;
import com.legadi.cli.jurl.parser.RequestParser;

public class ObjectsRegistry {

    private static final Map<Class<?>, List<Pair<Predicate<String>, Class<?>>>> EVALUABLES = new HashMap<>();
    private static final Map<Class<?>, Map<String, Class<?>>> NAMED = new HashMap<>();
    private static final Map<Class<?>, Object[]> CLASSES_ARGS = new HashMap<>();

    private static final Set<Class<?>> GROUP_CLASSES = new HashSet<>();
    private static final Set<Class<?>> REGISTERED_CLASSES = new HashSet<>();
    private static final Set<Class<?>> REGISTERED_BY_CLASS_NAMES = new HashSet<>();
    private static final Set<Class<?>> CONFIG_REPLACEABLE_CLASSES = new HashSet<>();

    static {
        register(Option.class, CleanOutputOption.class);
        register(Option.class, CurlPrintOption.class);
        register(Option.class, CustomClassOption.class);
        register(Option.class, DownloadInOption.class);
        register(Option.class, EnvironmentCopyOption.class);
        register(Option.class, EnvironmentOption.class);
        register(Option.class, ExecuteAuthenticationOption.class);
        register(Option.class, GetValueOption.class);
        register(Option.class, HelpOption.class);
        register(Option.class, ListRequestsOption.class);
        register(Option.class, MergeBodyOption.class);
        register(Option.class, MockRequestOption.class);
        register(Option.class, OpenEditorOption.class);
        register(Option.class, OverrideRequestOption.class);
        register(Option.class, RequestPrintOption.class);
        register(Option.class, RequestTypeOption.class);
        register(Option.class, SetFilterNameOption.class);
        register(Option.class, SetInputNameOption.class);
        register(Option.class, SetValueOption.class);
        register(Option.class, SkipAssertionsOption.class);
        register(Option.class, SkipAuthenticationOption.class);
        register(Option.class, SkipConditionsOption.class);
        register(Option.class, SkipUserInputOption.class);
        register(Option.class, StartInStepOption.class);
        register(Option.class, TimesRepeatOption.class);
        register(Option.class, UseAuthenticationOption.class);

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

        register(HeaderAuthenticator.class, HTTPTokenHeaderAuthenticator.class);
        register(HeaderAuthenticator.class, HTTPBasicHeaderAuthenticator.class);

        register(OutputReader.class, JsonOutputReader.class);
        register(OutputDecoder.class, GzipOutputDecoder.class);

        register(Generator.class, AlphaNumericGenerator.class);
        register(Generator.class, BooleanGenerator.class);
        register(Generator.class, DateTimeGenerator.class);
        register(Generator.class, DecimalGenerator.class);
        register(Generator.class, EmailGenerator.class);
        register(Generator.class, EmptyGenerator.class);
        register(Generator.class, EscapedGenerator.class);
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
        register(ValueModifier.class, Base64ValueModifier.class);
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

    public static void register(Class<?> groupClass, String typeClassName, Object... args) {
        Class<?> typeClass = typeOf(typeClassName);
        REGISTERED_BY_CLASS_NAMES.add(typeClass);
        register(groupClass, typeClass, args);
    }

    public static void register(Class<?> groupClass, Class<?> typeClass, Object... args) {
        boolean wasRegistered = false;

        if(typeClass.isAnnotationPresent(Evaluable.class)) {
            registerEvaluable(groupClass, typeClass, args);
            wasRegistered = true;
        }

        if(typeClass.isAnnotationPresent(Named.class)) {
            registerNamed(groupClass, typeClass, args);
            wasRegistered = true;
        }

        if(typeClass.isAnnotationPresent(ConfigReplaceable.class)) {
            CONFIG_REPLACEABLE_CLASSES.add(typeClass);
        }

        if(!wasRegistered) {
            throw new IllegalStateException("Type was not registered: " + typeClass);
        }

        GROUP_CLASSES.add(groupClass);
        REGISTERED_CLASSES.add(typeClass);

        if(isNotEmpty(args)) {
            CLASSES_ARGS.put(typeClass, args);
        }
    }

    public static boolean containsName(Class<?> groupClass, String name) {
        Class<?> typeClass = queryLastNamed(groupClass, name);
        return typeClass != null;
    }

    public static <T> Optional<T> findByName(Class<?> groupClass, String name) {
        return Optional.ofNullable(queryLastNamed(groupClass, name))
            .map(typeClass -> instantiate(typeClass, CLASSES_ARGS.get(typeClass)));
    }

    public static <T> T findByNameOrFail(Class<?> groupClass, String name) {
        Class<?> typeClass = queryLastNamed(groupClass, name);
        return validateAndInstantiate(groupClass, typeClass, name);
    }

    public static <T> Optional<T> find(Class<?> groupClass, String input) {
        return Optional.ofNullable(queryLastEvaluable(groupClass, input))
            .map(typeClass -> instantiate(typeClass, CLASSES_ARGS.get(typeClass)));
    }

    @SuppressWarnings("unchecked")
    public static <T> List<T> findAll(Class<?> groupClass, String input) {
        return queryAllEvaluable(groupClass, input)
            .stream()
            .map(typeClass -> instantiate(typeClass, CLASSES_ARGS.get(typeClass)))
            .map(instance -> (T) instance)
            .collect(Collectors.toCollection(ArrayList::new));
    }

    public static <T> T findOrFail(Class<?> groupClass, String input) {
        Class<?> typeClass = queryLastEvaluable(groupClass, input);
        return validateAndInstantiate(groupClass, typeClass, input);
    }

    public static <T> T findByTypeName(String typeClassName) {
        Class<?> typeClass = typeOf(typeClassName);
        return findByType(typeClass);
    }

    public static <T> T findByType(Class<?> typeClass) {
        if(!REGISTERED_CLASSES.contains(typeClass)) {
            throw new IllegalStateException("Unable to find type: " + typeClass);
        }
        return instantiate(typeClass, CLASSES_ARGS.get(typeClass));
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

    public static <T> List<T> getAllRegisteredByClassOf(Class<T> groupClass) {
        return getAllRegisteredPredicate(groupClass,
            typeClass -> !REGISTERED_BY_CLASS_NAMES.contains(typeClass));
    }

    public static <T> List<T> getAllRegisteredByNameOf(Class<T> groupClass) {
        return getAllRegisteredPredicate(groupClass,
            typeClass -> REGISTERED_BY_CLASS_NAMES.contains(typeClass));
    }

    public static List<Object> getAllRegisteredConfigReplaceable() {
        return CONFIG_REPLACEABLE_CLASSES
            .stream()
            .map(typeClass -> instantiate(typeClass, CLASSES_ARGS.get(typeClass)))
            .collect(Collectors.toList());
    }

    @SuppressWarnings("unchecked")
    private static <T> List<T> getAllRegisteredPredicate(Class<T> groupClass, Predicate<Class<?>> filter) {
        return entriesByGroup(groupClass)
            .stream()
            .filter(filter)
            .map(typeClass -> instantiate(typeClass, CLASSES_ARGS.get(typeClass)))
            .map(instance -> (T) instance)
            .collect(Collectors.toList());
    }

    private static void registerEvaluable(Class<?> groupClass, Class<?> typeClass, Object... args) {
        List<Pair<Predicate<String>, Class<?>>> entries = EVALUABLES.get(groupClass);

        if(entries == null) {
            entries = new ArrayList<>();
            EVALUABLES.put(groupClass, entries);
        }

        Evaluable evaluable = typeClass.getAnnotation(Evaluable.class);
        Predicate<String> evaluation = null;

        switch(evaluable.op()) {
            case CONTAINS:
                evaluation = input -> Arrays.stream(evaluable.values())
                    .anyMatch(v -> input.toLowerCase().contains(v.toLowerCase()));
                break;
            case ENDS_WITH:
                evaluation = input -> Arrays.stream(evaluable.values())
                    .anyMatch(v -> input.toLowerCase().endsWith(v.toLowerCase()));
                break;
            case EQUALS:
                evaluation = input -> Arrays.stream(evaluable.values())
                    .anyMatch(v -> input.equals(v));
                break;
            case EQUALS_IGNORE_CASE:
                evaluation = input -> Arrays.stream(evaluable.values())
                    .anyMatch(v -> input.equalsIgnoreCase(v));
                break;
            case STARTS_WITH:
                evaluation = input -> Arrays.stream(evaluable.values())
                    .anyMatch(v -> input.toLowerCase().startsWith(v.toLowerCase()));
                break;
            case ALWAYS_TRUE:
                evaluation = input -> true;
                break;
            default:
                evaluation = input -> false;
                break;
        }

        entries.add(new Pair<>(evaluation, typeClass));
    }

    private static void registerNamed(Class<?> groupClass, Class<?> typeClass, Object... args) {
        Map<String, Class<?>> entries = NAMED.get(groupClass);

        if(entries == null) {
            entries = new HashMap<>();
            NAMED.put(groupClass, entries);
        }

        Named named = typeClass.getAnnotation(Named.class);

        if(!named.allowOverride()) {
            if(entries.get(named.name().toLowerCase()) != null) {
                throw new IllegalStateException("Type with name '" + named.name() + "' was already registered: " + typeClass);
            }
            if(isNotBlank(named.alias()) && entries.get(named.alias().toLowerCase()) != null) {
                throw new IllegalStateException("Type with alias '" + named.alias() + "' was already registered: " + typeClass);
            }
        }

        entries.put(named.name().toLowerCase(), typeClass);

        if(isNotBlank(named.alias())) {
            entries.put(named.alias().toLowerCase(), typeClass);
        }
    }

    private static <T> T validateAndInstantiate(Class<?> groupClass, Class<?> typeClass, String input) {
        if(typeClass == null) {
            throw new CommandException("Unable to obtain a registered class for:" + groupClass + ":" + input);
        }

        return instantiate(typeClass, CLASSES_ARGS.get(typeClass));
    }

    private static Class<?> queryLastEvaluable(Class<?> groupClass, String input) {
        List<Class<?>> result = queryAllEvaluable(groupClass, input);

        if(result.isEmpty()) {
            return null;
        }

        return result.get(result.size() - 1);
    }

    private static List<Class<?>> queryAllEvaluable(Class<?> groupClass, String input) {
        return Optional.ofNullable(EVALUABLES.get(groupClass))
            .map(List::stream)
            .orElse(Stream.empty())
            .filter(p -> p.getLeft().test(input))
            .map(Pair::getRight)
            .collect(Collectors.toCollection(ArrayList::new));
    }

    private static Class<?> queryLastNamed(Class<?> groupClass, String name) {
        if(isBlank(name)) {
            return null;
        }
        return Optional.ofNullable(NAMED.get(groupClass))
            .map(classes -> classes.get(name.toLowerCase()))
            .orElse(null);
    }

    private static Set<Class<?>> entriesByGroup(Class<?> groupClass) {
        Set<Class<?>> entries = new HashSet<>();
        entries.addAll(Optional.ofNullable(EVALUABLES.get(groupClass))
            .map(List::stream)
            .orElse(Stream.empty())
            .map(Pair::getRight)
            .collect(Collectors.toSet())
        );
        entries.addAll(Optional.ofNullable(NAMED.get(groupClass))
            .map(Map::values)
            .map(Collection::stream)
            .orElse(Stream.empty())
            .collect(Collectors.toSet())
        );
        return entries;
    }
}
