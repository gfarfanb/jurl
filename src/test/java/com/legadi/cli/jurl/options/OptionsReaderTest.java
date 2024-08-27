package com.legadi.cli.jurl.options;

import java.time.Instant;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.ObjectsRegistry;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.CommandException;

public class OptionsReaderTest {

    @AfterEach
    public void cleanup() {
        Map<String, String> properties = new HashMap<>();
        properties.put("addOnOptionClasses", "");
        Settings.mergeProperties("default", properties);
    }
    
@Test
    public void extractOptionsAndRequestInputPathValidation() {
        OptionsReader reader = Assertions.assertDoesNotThrow(
            () -> new OptionsReader(
                new String[] { "-h", "-s", "field", "value", "src/test/resources/basic-functions.spec.http" }
            ));

        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", reader.getRequestInputPath());
        Assertions.assertEquals(2, reader.getOptionEntries().size());
        Assertions.assertDoesNotThrow(() -> (SetValueOption) reader.getOptionEntries().get(0).getLeft());
        Assertions.assertDoesNotThrow(() -> (HelpOption) reader.getOptionEntries().get(1).getLeft());
    }

    @Test
    public void extractOptionsAndRequestInputPathNoRequestPath() {
        OptionsReader reader = Assertions.assertDoesNotThrow(
            () -> new OptionsReader(
                new String[] { "-e", "local" }
            ));

        Assertions.assertNull(reader.getRequestInputPath());
        Assertions.assertDoesNotThrow(() -> (EnvironmentOption) reader.getOptionEntries().get(0).getLeft());
    }

    @Test
    public void extractOptionsAndRequestInputPathInvalidArgs() {
        ObjectsRegistry.register(Option.class, InvalidOption.class);

        Assertions.assertThrows(CommandException.class,
            () -> new OptionsReader(
                new String[] { "++invalid" }
            ));
    }

    @Test
    public void extractOptionsAndRequestInputPathNoArgs() {
        Assertions.assertThrows(CommandException.class,
            () -> new OptionsReader(null));
    }

    @Test
    public void registerAddOnOptionsValidation() {
        Map<String, String> properties = new HashMap<>();
        properties.put("addOnOptionClasses", SetDateOption.class.getName());
        Settings.mergeProperties("default", properties);

        OptionsReader readerAddOns = new OptionsReader(new String[] { "++set+date" });

        Assertions.assertEquals(1, readerAddOns.getOptionEntries().size());
        Assertions.assertDoesNotThrow(() -> (SetDateOption) readerAddOns.getOptionEntries().get(0).getLeft());
    }

    @Test
    public void registerAddOnOptionsEmptyOptions() {
        Map<String, String> properties = new HashMap<>();
        properties.put("addOnOptionClasses", " , , , ");
        Settings.mergeProperties("default", properties);

        OptionsReader reader = new OptionsReader(new String[] { "++not+found" });

        Assertions.assertTrue(reader.getOptionEntries().isEmpty());
    }

    public static class SetDateOption extends Option {

        @Override
        public String name() {
            return "++set+date";
        }

        @Override
        public String[] getArgs() {
            return new String[0];
        }

        @Override
        public String getDescription() {
            return "Set date to settings";
        }

        @Override
        public boolean execute(Settings settings, String[] args) {
            settings.putOverride(name(), Instant.now().toString());
            return true;
        }
    }

    public static class InvalidOption extends Option {

        @Override
        public String name() {
            return "++invalid";
        }

        @Override
        public String[] getArgs() {
            return null;
        }

        @Override
        public String getDescription() {
            return "Invalid option";
        }

        @Override
        public boolean execute(Settings settings, String[] args) {
            return true;
        }   
    }
}
