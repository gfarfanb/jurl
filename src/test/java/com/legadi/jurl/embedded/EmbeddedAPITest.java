package com.legadi.jurl.embedded;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.server.LocalServerPort;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.config.EmbeddedConfig;
import com.legadi.jurl.executor.RequestCommand;

@SpringBootTest(classes = EmbeddedConfig.class, webEnvironment = WebEnvironment.RANDOM_PORT)
public class EmbeddedAPITest {

    @LocalServerPort
    protected int port;

    public void jurl(String... args) {
        List<String> arguments = new ArrayList<>();
        arguments.addAll(Arrays.asList(args));

        Map<String, String> properties = new HashMap<>();
        properties.put("local.server.port", Integer.toString(port));
        Settings.mergeProperties(Settings.DEFAULT_ENVIRONMENT, properties);

        new RequestCommand(
            arguments.toArray(new String[arguments.size()])
        ).execute();
    }
}
