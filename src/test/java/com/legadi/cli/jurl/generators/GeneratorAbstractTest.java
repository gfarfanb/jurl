package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.ObjectsRegistry.findByName;

import java.util.Optional;

import com.legadi.cli.jurl.common.Settings;

public abstract class GeneratorAbstractTest {

    protected final String type;
    protected final Settings settings;

    public GeneratorAbstractTest(String type) {
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
