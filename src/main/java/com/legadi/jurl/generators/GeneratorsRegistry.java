package com.legadi.jurl.generators;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

import com.legadi.jurl.exception.CommandException;

public class GeneratorsRegistry {

    private static final Map<String, Supplier<Generator>> GENERATORS = new HashMap<>();

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

        if(GENERATORS.containsKey(generator.name())) {
            throw new CommandException("Generator [" + generator.name() + "] already exists");
        }

        GENERATORS.put(generator.name(), generatorSupplier);
    }

    public static Generator findGeneratorByName(String name) {
        Supplier<Generator> generator = GENERATORS.get(name);

        if(generator == null) {
            return null;
        }

        return generator.get();
    }
}
