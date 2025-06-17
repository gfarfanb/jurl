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
        Settings settings = new Settings();
        InputNameResolver inputNameResolver = new InputNameResolver(settings,
            "input-name-resolver.http", new RequestInput<>());
        String inputName = inputNameResolver.resolve("request-name");

        Assertions.assertEquals("request-name", inputName);
    }

    @Test
    public void resolverWithIndex() {
        Settings settings = new Settings();
        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        InputNameResolver inputNameResolver = new InputNameResolver(settings,
            "input-name-resolver.http", requestInput);
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
        Settings settings = new Settings();
        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        InputNameResolver inputNameResolver = new InputNameResolver(settings,
            "input-name-resolver.http", requestInput);

        requestInput.getRequests().put("request", new HTTPRequestEntry());
        requestInput.getFlows().put("flow", new FlowEntry());

        Assertions.assertThrows(CommandException.class, 
            () -> inputNameResolver.resolve(null));
    }

    @Test
    public void resolveWithBlank() {
        Settings settings = new Settings();
        InputNameResolver inputNameResolver = new InputNameResolver(settings,
            "input-name-resolver.http", new RequestInput<>());
        String inputName = inputNameResolver.resolve(null);

        Assertions.assertNull(inputName);
    }

    @Test
    public void resolveWithBlankAndDefault() {
        Settings settings = new Settings();
        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        InputNameResolver inputNameResolver = new InputNameResolver(settings,
            "input-name-resolver.http", requestInput);

        requestInput.getRequests().put("request", new HTTPRequestEntry());

        String inputName = inputNameResolver.resolve(null);

        Assertions.assertEquals("request", inputName);
    }

    @Test
    public void filterAndResolveInput() {
        Settings settings = new Settings();
        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        InputNameResolver inputNameResolver = new InputNameResolver(settings,
            "input-name-resolver.http", requestInput);
        String inputName;

        requestInput.getRequests().put("request-1", new HTTPRequestEntry());
        requestInput.getRequests().put("request-2", new HTTPRequestEntry());

        inputName = inputNameResolver.filterAndResolve("request-1", null);

        Assertions.assertEquals("request-1", inputName);

        inputName = inputNameResolver.filterAndResolve("request-1", "req");

        Assertions.assertEquals("request-1", inputName);

        Assertions.assertThrows(CommandException.class,
            () -> inputNameResolver.filterAndResolve(null, null));
    }

    @Test
    public void filterAndResolveFilter() {
        Settings settings = new Settings();
        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        InputNameResolver inputNameResolver = new InputNameResolver(settings,
            "input-name-resolver.http", requestInput);
        String inputName;

        requestInput.getRequests().put("request-1", new HTTPRequestEntry());
        requestInput.getRequests().put("request-2", new HTTPRequestEntry());
        requestInput.setDefaultRequest("request-2");

        inputName = inputNameResolver.filterAndResolve(null, "req");

        Assertions.assertEquals("request-2", inputName);

        inputName = inputNameResolver.filterAndResolve(null, "request-1");

        Assertions.assertEquals("request-1", inputName);

        inputName = inputNameResolver.filterAndResolve(null, "request-n");

        Assertions.assertEquals("request-n", inputName);
    }
}
