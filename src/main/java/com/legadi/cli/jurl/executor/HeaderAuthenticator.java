package com.legadi.cli.jurl.executor;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.AuthenticationEntry;
import com.legadi.cli.jurl.model.MockEntry;
import com.legadi.cli.jurl.model.RequestEntry;

public interface HeaderAuthenticator<T extends RequestEntry<? extends MockEntry>, E extends AuthenticationEntry> {

    boolean requiresExecution();

    Map<String, Field> getObjectFields();

    String getParserElement();

    @SuppressWarnings("unchecked")
    default <R extends RequestEntry<? extends MockEntry>> Supplier<E> getAuthEntrySupplier(
            Settings settings, R request) {
        return () -> {
            Optional<E> currentAuthEntry = Optional.ofNullable(request)
                .map(RequestEntry::getAuthEntries)
                .map(entries -> entries.get(getParserElement()))
                .map(auth -> (E) auth);
            if(currentAuthEntry.isPresent()) {
                return currentAuthEntry.get();
            } else {
                E authEntry = instanceAuthEntry(settings);
                request.getAuthEntries().put(authEntry.getParserElement(), authEntry);
                return authEntry;
            }
        };
    }

    E instanceAuthEntry(Settings settings);

    Optional<T> createAuthRequest(Settings settings, T api, T request,
        Map<String, Object> defaults);

    void cleanupAuth(Settings settings, T api, T request);

    void mergeAuthEntry(T api, T request);

    List<Pair<String, String>> getAuthHeaders(Settings settings, T request);

    @SuppressWarnings("unchecked")
    default Optional<E> findAuthEntry(T request) {
        return Optional.ofNullable(request)
            .map(RequestEntry::getAuthEntries)
            .map(entries -> entries.get(getParserElement()))
            .map(a -> (E) a);
    }
}
