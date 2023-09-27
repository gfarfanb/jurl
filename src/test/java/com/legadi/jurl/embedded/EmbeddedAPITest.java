package com.legadi.jurl.embedded;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.server.LocalServerPort;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.config.EmbeddedConfig;
import com.legadi.jurl.embedded.executor.HTTPRequestTestExecutor;
import com.legadi.jurl.embedded.executor.HTTPResponseTestProcessor;
import com.legadi.jurl.embedded.util.RequestCatcher;
import com.legadi.jurl.executor.RequestCommand;
import com.legadi.jurl.executor.RequestHandlersRegistry;

@SpringBootTest(classes = EmbeddedConfig.class, webEnvironment = WebEnvironment.RANDOM_PORT)
public abstract class EmbeddedAPITest {

    @LocalServerPort
    protected int port;

    @Autowired
    protected RequestCatcher requestCatcher;

    public UUID jurl(String... args) {
        UUID identifier = UUID.randomUUID();
        List<String> arguments = new ArrayList<>();
        arguments.addAll(Arrays.asList(args));

        Map<String, String> properties = new HashMap<>();
        properties.put("local.server.port", Integer.toString(port));
        Settings.mergeProperties(Settings.DEFAULT_ENVIRONMENT, properties);

        RequestHandlersRegistry.registerHandler(
            () -> new HTTPRequestTestExecutor(identifier, requestCatcher),
            () -> new HTTPResponseTestProcessor(identifier, requestCatcher)
        );

        new RequestCommand(arguments.toArray(new String[arguments.size()])).execute();

        return identifier;
    }
}
