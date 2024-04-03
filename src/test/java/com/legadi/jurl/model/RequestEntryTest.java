package com.legadi.jurl.model;

import java.util.LinkedHashMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.model.http.HTTPMockEntry;
import com.legadi.jurl.options.OptionsReader.OptionEntry;
import com.legadi.jurl.options.SetValueOption;

public class RequestEntryTest {

    @Test
    public void setterGetterValidation() {
        RequestEntry<HTTPMockEntry> model = new RequestEntry<>();

        model.setName("request");
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
    }
}
