package com.legadi.jurl.generators;

import static com.legadi.jurl.generators.GeneratorsRegistry.findGeneratorByName;

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
        Generator generator = findGeneratorByName(type);
        return generator.get(settings, arg);
    }
}
