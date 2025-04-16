package com.legadi.cli.jurl.executor.http;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

public class HTTPAuthEntryFactory {

    private final Settings settings;

    public HTTPAuthEntryFactory(Settings settings) {
        this.settings = settings;
    }

    public HTTPTokenAuthEntry instanceTokenAuth() {
        HTTPTokenAuthEntry authEntry = new HTTPTokenAuthEntry();
        authEntry.setGrantType(settings.getAuthBearerGrantType());
        return authEntry;
    }
}
