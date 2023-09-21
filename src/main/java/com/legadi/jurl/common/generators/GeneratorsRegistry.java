package com.legadi.jurl.common.generators;

import java.util.LinkedList;
import java.util.List;

import com.legadi.jurl.common.Settings;

public class GeneratorsRegistry {

    private static final List<Generator> GENERATORS = new LinkedList<>();

    static {
        GENERATORS.add(new AlphaNumericGenerator());
        GENERATORS.add(new BooleanGenerator());
        GENERATORS.add(new DecimalGenerator());
        GENERATORS.add(new EmailGenerator());;
        GENERATORS.add(new FullNameGenerator());
        GENERATORS.add(new IntegerGenerator());
        GENERATORS.add(new LastNameGenerator());
        GENERATORS.add(new LoremIpsumGenerator());
        GENERATORS.add(new NameGenerator());
        GENERATORS.add(new PasswordInputGenerator());
        GENERATORS.add(new PickAnyGenerator());
        GENERATORS.add(new UserInputGenerator());
        GENERATORS.add(new UUIDGenerator());
    }

    public static String getValueByGenerator(Settings settings, String param) {
        return GENERATORS
            .stream()
            .filter(generator -> generator.accepts(settings, param))
            .findFirst()
            .map(generator -> generator.get(settings, param))
            .orElse(null);
    }
}
