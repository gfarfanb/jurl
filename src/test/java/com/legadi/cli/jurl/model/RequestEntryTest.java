package com.legadi.cli.jurl.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.model.http.HTTPMockEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPBasicAuthEntry;
import com.legadi.cli.jurl.options.OptionsReader.OptionEntry;
import com.legadi.cli.jurl.options.SetValueOption;

public class RequestEntryTest {

    @Test
    public void setterGetterValidation() {
        RequestEntry<HTTPMockEntry> model = new RequestEntry<>();

        model.setName("request");
        model.setDescription("Request entry");
        model.setUrl("http://localhost:42121/basic/body");
        model.setProtocol("http");
        model.setHost("localhost");
        model.setPort("42121");
        model.setBasePath("basic");
        model.setEndpoint("body");
        model.setMockDefinition(new HTTPMockEntry());

        List<AssertionEntry> conditions = new ArrayList<>();
        conditions.add(new AssertionEntry());
        model.setConditions(conditions);

        Map<String, String> outputMappings = new LinkedHashMap<>();
        outputMappings.put("property.name", "{{OUT/name}}");
        model.setOutputMappings(outputMappings);

        List<AssertionEntry> assertions = new ArrayList<>();
        assertions.add(new AssertionEntry());
        model.setAssertions(assertions);

        List<OptionEntry> options = new ArrayList<>();
        options.add(new OptionEntry(new SetValueOption(), new String[] { "field", "value" }));
        model.setOptions(options);

        Assertions.assertEquals("request", model.getName());
        Assertions.assertEquals("Request entry", model.getDescription());
        Assertions.assertEquals("http://localhost:42121/basic/body", model.getUrl());
        Assertions.assertEquals("http", model.getProtocol());
        Assertions.assertEquals("localhost", model.getHost());
        Assertions.assertEquals("42121", model.getPort());
        Assertions.assertEquals("basic", model.getBasePath());
        Assertions.assertEquals("body", model.getEndpoint());
        Assertions.assertNotNull(model.getMockDefinition());
        Assertions.assertEquals(1, model.getConditions().size());
        Assertions.assertEquals("{{OUT/name}}", model.getOutputMappings().get("property.name"));
        Assertions.assertEquals(1, model.getAssertions().size());
        Assertions.assertEquals(1, model.getOptions().size());
        Assertions.assertEquals(SetValueOption.class, model.getOptions().get(0).getLeft().getClass());

        Assertions.assertDoesNotThrow(() -> model.getDefaults().put("property.first", "2342"));
        Assertions.assertDoesNotThrow(() -> model.getDefaults().put("property.second", "5"));
        Assertions.assertDoesNotThrow(() -> model.getDefaults().put("property.third", "255.0"));

        Assertions.assertEquals(3, model.getDefaults().size());
        Assertions.assertEquals(Arrays.asList("property.first", "property.second", "property.third"),
            model.getDefaults().keySet().stream().collect(Collectors.toList()));
        Assertions.assertEquals("2342", model.getDefaults().get("property.first"));
        Assertions.assertEquals("5", model.getDefaults().get("property.second"));
        Assertions.assertEquals("255.0", model.getDefaults().get("property.third"));

        model.setDefaults(new HashMap<>());
        Assertions.assertNotNull(model.getDefaults());

        Assertions.assertDoesNotThrow(() -> model.getAuthEntries().put("basic", new HTTPBasicAuthEntry()));
        model.setAuthEntries(new HashMap<>());
        Assertions.assertNotNull(model.getAuthEntries());
    }
}
