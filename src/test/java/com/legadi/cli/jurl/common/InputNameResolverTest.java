package com.legadi.cli.jurl.common;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.FlowEntry;
import com.legadi.cli.jurl.model.RequestInput;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;

public class InputNameResolverTest {

    @Test
    public void resolveWithNotBlank() {
        InputNameResolver inputNameResolver = new InputNameResolver("input-name-resolver.http", new RequestInput<>());
        String inputName = inputNameResolver.resolve("request-name");

        Assertions.assertEquals("request-name", inputName);
    }

    @Test
    public void resolverWithIndex() {
        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        InputNameResolver inputNameResolver = new InputNameResolver("input-name-resolver.http", requestInput);
        String inputName;

        requestInput.getRequests().put("request", new HTTPRequestEntry());
        requestInput.getFlows().put("flow", new FlowEntry());

        requestInput.setDefaultRequest("request");
        inputName = inputNameResolver.resolve(null);

        Assertions.assertEquals("request", inputName);

        requestInput.setDefaultRequest("flow");
        inputName = inputNameResolver.resolve(null);

        Assertions.assertEquals("flow", inputName);
    }

    @Test
    public void resolverInvalidIndex() {
        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        InputNameResolver inputNameResolver = new InputNameResolver("input-name-resolver.http", requestInput);

        requestInput.getRequests().put("request", new HTTPRequestEntry());
        requestInput.getFlows().put("flow", new FlowEntry());

        Assertions.assertThrows(CommandException.class, 
            () -> inputNameResolver.resolve(null));
    }
}
