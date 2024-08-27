package com.legadi.cli.jurl.embedded;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.server.LocalServerPort;

import com.legadi.cli.jurl.common.ObjectsRegistry;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.config.EmbeddedConfig;
import com.legadi.cli.jurl.embedded.executor.HTTPRequestTestExecutor;
import com.legadi.cli.jurl.embedded.executor.HTTPResponseTestProcessor;
import com.legadi.cli.jurl.embedded.util.RequestCatcher;
import com.legadi.cli.jurl.embedded.util.RequestCatcherManager;
import com.legadi.cli.jurl.executor.RequestCommand;
import com.legadi.cli.jurl.executor.RequestExecutor;
import com.legadi.cli.jurl.executor.ResponseProcessor;

@SpringBootTest(classes = EmbeddedConfig.class, webEnvironment = WebEnvironment.RANDOM_PORT)
public abstract class EmbeddedAPITest {

    @LocalServerPort
    protected int port;

    protected final String requestCatcherId;
    protected final RequestCatcher requestCatcher;

    public EmbeddedAPITest() {
        this.requestCatcherId = UUID.randomUUID().toString();
        this.requestCatcher = RequestCatcherManager.getCatcher(requestCatcherId);
    }

    public UUID jurl(String... args) {
        UUID correlationId = UUID.randomUUID();
        return jurl(correlationId, args);
    }

    public UUID jurl(UUID correlationId, String... args) {
        List<String> arguments = new ArrayList<>();
        arguments.addAll(Arrays.asList(args));

        Map<String, String> properties = new HashMap<>();
        properties.put("local.server.port", Integer.toString(port));
        properties.put("request.catcher.identifier", requestCatcherId);
        Settings.mergeProperties(Settings.DEFAULT_ENVIRONMENT, properties);

        ObjectsRegistry.register(RequestExecutor.class, 
            HTTPRequestTestExecutor.class, correlationId, requestCatcher);
        ObjectsRegistry.register(ResponseProcessor.class,
            HTTPResponseTestProcessor.class, correlationId, requestCatcher);

        new RequestCommand(arguments.toArray(new String[arguments.size()])).execute();

        return correlationId;
    }
}
