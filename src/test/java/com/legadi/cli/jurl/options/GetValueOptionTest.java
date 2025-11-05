package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.JsonUtils.loadInternalJsonProperties;
import static com.legadi.cli.jurl.common.SettingsConstants.DEFAULT_SETTINGS_FILE;

import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.exception.SkipExecutionException;

public class GetValueOptionTest extends OptionAbstractTest<GetValueOption> {

    public GetValueOptionTest() {
        super(GetValueOption.class, false);
    }

    @Test
    public void setValueValidation() {
        Map<String, String> defaultSettings = loadInternalJsonProperties(DEFAULT_SETTINGS_FILE);

        for(String property : defaultSettings.keySet()) {
            Assertions.assertThrows(SkipExecutionException.class,
                () -> jurl("-g", property));
        }

        Assertions.assertThrows(CommandException.class,
            () -> jurl("-g", "property.not.found"));
    }
}
