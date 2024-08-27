package com.legadi.cli.jurl.model;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.options.OptionsReader.OptionEntry;
import com.legadi.cli.jurl.options.SetValueOption;

public class StepEntryTest {

    @Test
    public void setterGetterValidation() {
        StepEntry model = new StepEntry();

        model.setRequestInputPath("src/test/resources/basic-functions.spec.http");
        
        List<OptionEntry> options = new ArrayList<>();
        options.add(new OptionEntry(new SetValueOption(), new String[] { "field", "value" }));
        model.setOptions(options);

        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", model.getRequestInputPath());
        Assertions.assertEquals(1, model.getOptions().size());
        Assertions.assertEquals(SetValueOption.class, model.getOptions().get(0).getLeft().getClass());
    }
}
