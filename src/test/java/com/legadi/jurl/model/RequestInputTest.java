package com.legadi.jurl.model;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.model.http.HTTPRequestEntry;

public class RequestInputTest {

    @Test
    public void setterGetterValidation() {
        RequestInput<HTTPRequestEntry> model = new RequestInput<>();

        model.setDefaultRequest("create");
        model.setApi(new HTTPRequestEntry());

        Map<String, String> config = new HashMap<>();
        config.put("local.port", "2342");
        model.setConfig(config);

        Map<String, HTTPRequestEntry> requests = new HashMap<>();
        requests.put("create", new HTTPRequestEntry());
        model.setRequests(requests);

        Map<String, List<StepEntry>> flows = new HashMap<>();
        List<StepEntry> steps = new LinkedList<>();
        steps.add(new StepEntry());
        flows.put("createWithAuth", steps);
        model.setFlows(flows);

        Assertions.assertEquals("create", model.getDefaultRequest());
        Assertions.assertNotNull(model.getApi());
        Assertions.assertEquals(1, model.getConfig().size());
        Assertions.assertEquals("2342", model.getConfig().get("local.port"));
        Assertions.assertNotNull(model.getRequests().get("create"));
        Assertions.assertNotNull(model.getFlows().get("createWithAuth"));
        Assertions.assertEquals(1, model.getFlows().get("createWithAuth").size());
    }
}
