package com.legadi.cli.jurl.model;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.model.http.HTTPRequestEntry;

public class RequestInputTest {

    @Test
    public void setterGetterValidation() {
        RequestInput<HTTPRequestEntry> model = new RequestInput<>();

        model.setDefaultRequest("create");
        model.setApi(new HTTPRequestEntry());

        Map<String, HTTPRequestEntry> requests = new HashMap<>();
        requests.put("create", new HTTPRequestEntry());
        model.setRequests(requests);

        Map<String, FlowEntry> flows = new HashMap<>();
        FlowEntry flow = new FlowEntry();
        flow.getSteps().add(new StepEntry());
        flows.put("createWithAuth", flow);
        model.setFlows(flows);

        Assertions.assertEquals("create", model.getDefaultRequest());
        Assertions.assertNotNull(model.getApi());
        Assertions.assertNotNull(model.getRequests().get("create"));
        Assertions.assertNotNull(model.getFlows().get("createWithAuth"));
        Assertions.assertEquals(1, model.getFlows().get("createWithAuth").getSteps().size());
    }
}
