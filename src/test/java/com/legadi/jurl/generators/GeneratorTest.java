package com.legadi.jurl.generators;

import static com.legadi.jurl.generators.GeneratorsRegistry.getValueByParam;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.model.GeneratorType;

public abstract class GeneratorTest {

    protected final GeneratorType type;
    protected final Settings settings;

    public GeneratorTest(GeneratorType type) {
        this.type = type;
        this.settings = new Settings();
    }

    public String generate() {
        return generate(null);
    }

    public String generate(String arg) {
        if(arg != null) {
            return getValueByParam(settings, type.tag() + arg);
        } else {
            return getValueByParam(settings, type.tag());
        }
    }
}
