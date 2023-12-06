package com.legadi.jurl.generators;

import static com.legadi.jurl.common.ObjectsRegistry.findByName;

import java.util.Optional;

import com.legadi.jurl.common.Settings;

public abstract class GeneratorTest {

    protected final String type;
    protected final Settings settings;

    public GeneratorTest(String type) {
        this.type = type;
        this.settings = new Settings();
    }

    public String generate() {
        return generate(null);
    }

    public String generate(String arg) {
        Optional<Generator> generatorOptional = findByName(Generator.class, type);
        return generatorOptional
            .map(generator -> generator.get(settings, arg))
            .orElse(null);
    }
}
