package com.legadi.cli.jurl.embedded.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class RequestCatcherManager {

    private static final Map<UUID, RequestCatcher> CATCHERS = Collections.synchronizedMap(new HashMap<>());

    public static RequestCatcher getCatcher(String rawId) {
        UUID identifier = UUID.fromString(rawId);
        RequestCatcher catcher = CATCHERS.get(identifier);
        if(catcher == null) {
            catcher = new RequestCatcher();
            CATCHERS.put(identifier, catcher);
        }
        return catcher;
    }
}
