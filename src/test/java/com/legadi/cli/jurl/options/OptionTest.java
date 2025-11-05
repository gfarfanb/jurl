package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedAlias;
import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedName;

import java.util.Objects;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

public class OptionTest {

    @Test
    public void validateObjectMethods() {
        Option option = new HelpOption();
        Assertions.assertEquals(Objects.hash(extractNamedName(option), extractNamedAlias(option)), option.hashCode());
        Assertions.assertTrue(option.equals(option));
        Assertions.assertTrue(option.equals(new HelpOption()));
        Assertions.assertFalse(option.equals(new Object()));
        Assertions.assertFalse(option.equals(new NamedOption()));
        Assertions.assertFalse(option.equals(new AlmostHelpOption()));
        Assertions.assertTrue(option.toString().startsWith(extractNamedName(option) + ", " + extractNamedAlias(option)));
    }

    @Named(name = "lhn345lj6n45lj45ptn3l45knet5", alias = "34jn345j3n4krjn4k3n4")
    public static class NamedOption extends ImplementedOption { }

    @Named(name = "--help", alias = "++h")
    public static class AlmostHelpOption extends ImplementedOption { }

    public static class ImplementedOption extends Option {

        @Override
        public String[] getArgs() {
            return new String[0];
        }

        @Override
        public String getDescription() {
            return null;
        }

        @Override
        public boolean execute(Settings settings, String[] args) {
            return true;
        }
    }
}
