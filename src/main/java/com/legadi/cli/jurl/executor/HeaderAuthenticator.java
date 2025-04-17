package com.legadi.cli.jurl.executor;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import com.legadi.cli.jurl.common.Evaluable;
import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.AuthenticationEntry;
import com.legadi.cli.jurl.model.MockEntry;
import com.legadi.cli.jurl.model.RequestEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;

public interface HeaderAuthenticator<T extends RequestEntry<? extends MockEntry>, E extends AuthenticationEntry>
        extends Evaluable {

    @Override
    default boolean accepts(String name) {
        return type().equalsIgnoreCase(name);
    }

    String type();

    boolean requiresExecution();

    Map<String, Field> getObjectFields();

    String getParserElement();

    @SuppressWarnings("unchecked")
    default <R extends RequestEntry<? extends MockEntry>> Supplier<E> getAuthEntrySupplier(
            Settings settings, R request) {
        return () -> {
            Optional<E> currentAuthEntry = request.getAuthEntries()
                .stream()
                .filter(auth -> auth.getParserElement().equalsIgnoreCase(getParserElement()))
                .map(auth -> (E) auth)
                .findFirst();
            if(currentAuthEntry.isPresent()) {
                return currentAuthEntry.get();
            } else {
                E authEntry = instanceAuthEntry(settings);
                request.getAuthEntries().add(authEntry);
                return authEntry;
            }
        };
    }

    E instanceAuthEntry(Settings settings);

    Optional<HTTPRequestEntry> createAuthRequest(Settings settings, T api, T request,
        Map<String, Object> defaults);

    void cleanupAuth(Settings settings, T api, T request);

    void mergeAuthEntry(T api, T request);

    List<Pair<String, String>> getAuthHeaders(Settings settings, T request);
}
