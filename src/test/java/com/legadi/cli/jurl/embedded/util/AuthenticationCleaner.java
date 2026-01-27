package com.legadi.cli.jurl.embedded.util;

import static com.legadi.cli.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.cli.jurl.common.JsonUtils.removeJsonProperties;

import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.legadi.cli.jurl.common.Settings;

public class AuthenticationCleaner {

    private AuthenticationCleaner() {}

    public static void cleanup() {
        Settings settings = new Settings();
        Path overridePath = settings.getOverrideFilePath();
        Map<String, String> overrideProperties = loadJsonProperties(overridePath);
        Set<String> authParams = overrideProperties.keySet()
            .stream()
            .filter(AuthenticationCleaner::isAuthParam)
            .collect(Collectors.toSet());

        removeProperties(settings, authParams);
    }

    private static void removeProperties(Settings settings, Collection<String> keys) {
        String[] authParams = keys.toArray(new String[keys.size()]);

        removeJsonProperties(settings.getOverrideFilePath(), authParams);
        settings.removeProperties(authParams);
    }

    private static boolean isAuthParam(String key) {
        return key.contains("expires-in.")
            || key.contains("expiration-millis")
            || key.contains("access-token")
            || key.contains("token-type")
            || key.contains("expiration-date");
    }
}
