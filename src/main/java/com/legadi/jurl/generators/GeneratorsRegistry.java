package com.legadi.jurl.generators;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;

public class GeneratorsRegistry {

    private static final List<Pair<Predicate<String>, Supplier<Generator>>> GENERATORS = new LinkedList<>();

    static {
        registerGenerator(AlphaNumericGenerator::new);
        registerGenerator(BooleanGenerator::new);
        registerGenerator(DateTimeGenerator::new);
        registerGenerator(DecimalGenerator::new);
        registerGenerator(EmailGenerator::new);;
        registerGenerator(FullNameGenerator::new);
        registerGenerator(IntegerGenerator::new);
        registerGenerator(LastNameGenerator::new);
        registerGenerator(LoremIpsumGenerator::new);
        registerGenerator(NameGenerator::new);
        registerGenerator(PasswordInputGenerator::new);
        registerGenerator(PickAnyGenerator::new);
        registerGenerator(UserInputGenerator::new);
        registerGenerator(UUIDGenerator::new);
    }

    private GeneratorsRegistry() {}

    public static void registerGenerator(String generatorClass) {
        registerGenerator(() -> instantiate(generatorClass));
    }

    public static void registerGenerator(Supplier<Generator> generatorSupplier) {
        Generator generator = generatorSupplier.get();
        GENERATORS.add(new Pair<>(param -> generator.accepts(param), generatorSupplier));
    }

    public static String getValueByParam(Settings settings, String param) {
        List<Generator> generators = GENERATORS
            .stream()
            .filter(p -> p.getLeft().test(param))
            .map(Pair::getRight)
            .map(Supplier::get)
            .collect(Collectors.toCollection(ArrayList::new));

        if(generators.isEmpty()) {
            return null;
        }

        Generator lastGenerator = generators.get(generators.size() - 1);
        return lastGenerator.get(settings, param);
    }
}
