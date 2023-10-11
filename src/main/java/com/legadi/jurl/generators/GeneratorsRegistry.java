package com.legadi.jurl.generators;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Settings;

public class GeneratorsRegistry {

    private static final List<Supplier<Generator>> GENERATORS = new LinkedList<>();

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

    public static void registerGenerator(String generatorClass) {
        registerGenerator(() -> instantiate(generatorClass));
    }

    public static void registerGenerator(Supplier<Generator> generatorSupplier) {
        GENERATORS.add(generatorSupplier);
    }

    public static String getValueByParam(Settings settings, String param) {
        List<Generator> generators = GENERATORS
            .stream()
            .map(Supplier::get)
            .filter(generator -> generator.accepts(param))
            .collect(Collectors.toCollection(ArrayList::new));

        if(generators.isEmpty()) {
            return null;
        }

        Generator lastGenerator = generators.get(generators.size() - 1);
        return lastGenerator.get(settings, param);
    }
}
