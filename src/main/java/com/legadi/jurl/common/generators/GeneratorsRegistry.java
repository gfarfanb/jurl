package com.legadi.jurl.common.generators;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;

public class GeneratorsRegistry {

    private static final List<Generator> GENERATORS = new LinkedList<>();
    private static final Map<String, Generator> REGISTERED = new HashMap<>();

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

    public static void registerGenerator(String generatorClass) {
        if(REGISTERED.containsKey(generatorClass)) {
            throw new CommandException("Generator [" + generatorClass + "] already exists");
        }

        Generator generator = instantiate(generatorClass);
        GENERATORS.add(generator);
        REGISTERED.put(generatorClass, generator);
    }

    public static String getValueByGenerator(Settings settings, String param) {
        List<Generator> generators = GENERATORS
            .stream()
            .filter(generator -> generator.accepts(settings, param))
            .collect(Collectors.toCollection(ArrayList::new));

        if(generators.isEmpty()) {
            return null;
        }

        Generator lastGenerator = generators.get(generators.size() - 1);

        return lastGenerator.get(settings, param);
    }
}
