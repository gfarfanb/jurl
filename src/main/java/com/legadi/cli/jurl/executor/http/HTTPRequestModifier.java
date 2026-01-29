package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedName;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findAll;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findOrFail;
import static com.legadi.cli.jurl.common.WriterUtils.writeFile;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import com.legadi.cli.jurl.common.OutputPathBuilder;
import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.StringExpander;
import com.legadi.cli.jurl.common.annotations.Named;
import com.legadi.cli.jurl.exception.RequestException;
import com.legadi.cli.jurl.executor.HeaderAuthenticator;
import com.legadi.cli.jurl.executor.RequestModifier;
import com.legadi.cli.jurl.executor.mixer.BodyMixer;
import com.legadi.cli.jurl.executor.mixer.BodyMixer.MixerEntry;
import com.legadi.cli.jurl.model.AssertionEntry;
import com.legadi.cli.jurl.model.AuthenticationEntry;
import com.legadi.cli.jurl.model.FlowEntry;
import com.legadi.cli.jurl.model.RequestInput;
import com.legadi.cli.jurl.model.StepEntry;
import com.legadi.cli.jurl.model.http.HTTPMockEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;
import com.legadi.cli.jurl.options.OptionsReader.OptionEntry;
import com.legadi.cli.jurl.parser.HTTPRequestParser;
import com.legadi.cli.jurl.parser.RequestParser;

@Named(name = "http", allowOverride = true)
public class HTTPRequestModifier implements RequestModifier<HTTPRequestEntry, HTTPResponseEntry> {

    public static final String BODY_TEMPORAL_PATH = "http.request.executor/body.temporal.path";

    @Override
    public List<HTTPRequestEntry> getAuthenticationDefinition(
            String requestName, RequestInput<HTTPRequestEntry> requestInput,
            Settings settings, BiConsumer<Settings, List<OptionEntry>> optionsProcessor) {
        HTTPRequestEntry request = requestInput.getRequests().get(requestName);

        processOptionFlags(settings, request, optionsProcessor);

        if(settings.isSkipAuthentication()) {
            return new ArrayList<>();
        }

        List<HeaderAuthenticator<HTTPRequestEntry, ?>> headerAuthenticators = findAll(
            HeaderAuthenticator.class, extractNamedName(this));
        List<HTTPRequestEntry> authRequests = new ArrayList<>();
        HTTPRequestEntry api = requestInput.getApi();

        Map<String, Object> defaults = Optional.ofNullable(api)
            .map(HTTPRequestEntry::getDefaults)
            .map(HashMap::new)
            .orElse(new HashMap<>());
        defaults.putAll(request.getDefaults());

        StringExpander stringExpander = new StringExpander(settings, defaults);
        expandDefaults(stringExpander, defaults);

        for(HeaderAuthenticator<HTTPRequestEntry, ?> headerAuthenticator : headerAuthenticators) {
            if(!headerAuthenticator.requiresExecution()) {
                continue;
            }

            headerAuthenticator.createAuthRequest(settings, api, request, defaults)
                .ifPresent(authRequests::add);

            if(settings.isExecuteAuthentication()) {
                headerAuthenticator.cleanupAuth(settings, api, request);
            }
        }

        return authRequests;
    }

    @Override
    public void mergeRequestHeader(HTTPRequestEntry api, HTTPRequestEntry request) {
        if(isBlank(request.getUrl())) {
            request.setUrl(api.getUrl());
        }
    }

    @Override
    public void mergeAPIDefinition(Settings settings, HTTPRequestEntry api, HTTPRequestEntry request) {
        if(api == null || request == null) {
            return;
        }

        mergeRequestHeader(api, request);

        if(request.getMockDefinition() == null) {
            request.setMockDefinition(api.getMockDefinition());
        } else if(api.getMockDefinition() != null) {
            mergeMockDefinition(api.getMockDefinition(), request.getMockDefinition());
        }

        List<AssertionEntry> conditions = new ArrayList<>(api.getConditions());
        conditions.addAll(request.getConditions());
        request.setConditions(conditions);

        Map<String, String> outputMappings = new LinkedHashMap<>(api.getOutputMappings());
        outputMappings.putAll(request.getOutputMappings());
        request.setOutputMappings(outputMappings);

        List<AssertionEntry> assertions = new ArrayList<>(api.getAssertions());
        assertions.addAll(request.getAssertions());
        request.setAssertions(assertions);

        List<OptionEntry> options = new ArrayList<>(api.getOptions());
        options.addAll(request.getOptions());
        request.setOptions(options);

        if(isBlank(request.getMethod())) {
            request.setMethod(api.getMethod());
        }

        Map<String, String> queryParams = new HashMap<>(api.getQueryParams());
        queryParams.putAll(request.getQueryParams());
        request.setQueryParams(queryParams);

        Map<String, String> headers = new HashMap<>(api.getHeaders());
        headers.putAll(request.getHeaders());
        request.setHeaders(headers);

        if(isBlank(request.getBodyCharset())) {
            request.setBodyCharset(api.getBodyCharset());
        }
        if(isBlank(request.getBodyContent())) {
            request.setBodyContent(api.getBodyContent());
        }
        if(isBlank(request.getBodyFilePath())) {
            request.setBodyFilePath(api.getBodyFilePath());
        }
        if(isBlank(request.getBodyMergePath())) {
            request.setBodyMergePath(api.getBodyMergePath());
        }
        if(isBlank(request.getBodyMergeAsBase())) {
            request.setBodyMergeAsBase(api.getBodyMergeAsBase());
        }

        mergeRequestFiles(api.getRequestFiles(), request.getRequestFiles());

        Map<String, String> formData = new HashMap<>(api.getFormData());
        formData.putAll(request.getFormData());
        request.setFormData(formData);

        Map<String, Object> defaults = new HashMap<>(api.getDefaults());
        defaults.putAll(request.getDefaults());
        request.setDefaults(defaults);

        List<HeaderAuthenticator<HTTPRequestEntry, ?>> headerAuthenticators = findAll(
            HeaderAuthenticator.class, extractNamedName(this));

        for(HeaderAuthenticator<HTTPRequestEntry, ?> headerAuthenticator : headerAuthenticators) {
            Optional<? extends AuthenticationEntry> apiAuth = headerAuthenticator.findAuthEntry(api);
            Optional<? extends AuthenticationEntry> requestAuth = headerAuthenticator.findAuthEntry(request);

            if(!requestAuth.isPresent()) {
                apiAuth.ifPresent(auth -> request.getAuthEntries().put(auth.getParserElement(), auth));
            } else if(apiAuth.isPresent()) {
                headerAuthenticator.mergeAuthEntry(api, request);
            }
        }
    }

    @Override
    public void mergeBodyFileWithBodyContent(Settings settings, String requestPath, HTTPRequestEntry request) {
        if(isBlank(request.getBodyContent()) && isBlank(request.getBodyFilePath())) {
            throw new RequestException(request, "request.bodyContent or request.bodyFilePath is null or empty");
        }
        if(isBlank(request.getBodyMergePath())) {
            throw new RequestException(request, "request.bodyMergePath is null or empty");
        }

        if(isNotBlank(request.getBodyContent())) {
            OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
                .setRequestPath(requestPath)
                .setRequestName(request.getName())
                .setExtension("body");
            Path temporalBodyPath = pathBuilder.buildCommandPath();

            writeFile(temporalBodyPath, request.getBodyContent());
            request.setBodyFilePath(temporalBodyPath.toString());
        }

        BodyMixer mixer = findOrFail(BodyMixer.class, settings.getMergeBodyUsingType());
        MixerEntry mixerEntry = new MixerEntry()
                .setRequestPath(requestPath)
                .setRequestName(request.getName());

        if(Boolean.parseBoolean(request.getBodyMergeAsBase())) {
            mixerEntry
                .setBodyBasePath(request.getBodyMergePath())
                .setBodyComparePath(request.getBodyFilePath());
        } else {
            mixerEntry
                .setBodyBasePath(request.getBodyFilePath())
                .setBodyComparePath(request.getBodyMergePath());
        }

        Path temporalMixedBodyPath = mixer.apply(settings, request.getDefaults(), mixerEntry);

        request.setBodyContent(null);
        request.setBodyFilePath(null);
        request.setBodyMergePath(null);

        settings.putOverride(BODY_TEMPORAL_PATH, temporalMixedBodyPath.toString());
    }

    @Override
    public void overrideRequestWithFile(Settings settings, HTTPRequestEntry request, String filename) {
        HTTPRequestParser parser = findOrFail(
            RequestParser.class, extractNamedName(this));
        HTTPRequestEntry overrideRequest = parser.parseRequest(settings, Paths.get(filename));

        overrideRequest.getDefaults().putAll(request.getDefaults());
        expandRequest(settings, overrideRequest);

        request.getHeaders().putAll(overrideRequest.getHeaders());
        request.getQueryParams().putAll(overrideRequest.getQueryParams());

        if(isNotBlank(overrideRequest.getBodyContent())) {
            request.setBodyContent(overrideRequest.getBodyContent());
        }

        if(isNotBlank(overrideRequest.getBodyFilePath())) {
            request.setBodyFilePath(overrideRequest.getBodyFilePath());
        }
        if(isNotBlank(overrideRequest.getBodyMergePath())) {
            request.setBodyMergePath(overrideRequest.getBodyMergePath());
        }
        if(isNotBlank(overrideRequest.getBodyMergeAsBase())) {
            request.setBodyMergeAsBase(overrideRequest.getBodyMergeAsBase());
        }
    }

    @Override
    public void expandFlow(Settings settings, FlowEntry flow) {
        StringExpander stringExpander = new StringExpander(settings, flow.getDefaults());

        expandDefaults(stringExpander, flow.getDefaults());

        for(StepEntry step : flow.getSteps()) {
            step.setRequestInputPath(stringExpander.replaceAllInContent(step.getRequestInputPath()));
            step.getOptions().stream().map(OptionEntry::getRight)
                .forEach(args -> expandArray(stringExpander, args));
        }
    }

    @Override
    public void expandRequestDefinition(Settings settings, HTTPRequestEntry request) {
        StringExpander stringExpander = new StringExpander(settings, request.getDefaults());

        expandDefaults(stringExpander, request.getDefaults());

        request.setUrl(stringExpander.replaceAllInContent(request.getUrl()));

        request.getConditions().forEach(condition -> expandAssertion(stringExpander, condition));
        request.getOptions().stream().map(Pair::getRight).forEach(args -> expandArray(stringExpander, args));

        request.setMethod(stringExpander.replaceAllInContent(request.getMethod()));
        expandMap(stringExpander, request.getQueryParams());
        expandMap(stringExpander, request.getHeaders());
        request.setBodyCharset(stringExpander.replaceAllInContent(request.getBodyCharset()));
        request.setBodyContent(stringExpander.replaceAllInContent(request.getBodyContent()));
        request.setBodyFilePath(stringExpander.replaceAllInContent(request.getBodyFilePath()));
        request.setBodyMergePath(stringExpander.replaceAllInContent(request.getBodyMergePath()));
        request.setBodyMergeAsBase(stringExpander.replaceAllInContent(request.getBodyMergeAsBase()));
        request.getRequestFiles().forEach(requestFile -> expandRequestFile(stringExpander, requestFile));
        expandMap(stringExpander, request.getFormData());
    }

    private void processOptionFlags(Settings settings, HTTPRequestEntry request,
            BiConsumer<Settings, List<OptionEntry>> optionsProcessor) {
        List<OptionEntry> optionEntries = request.getOptions()
            .stream()
            .filter(opt -> opt.getLeft().requiredForAuth())
            .collect(Collectors.toList());
        Optional.ofNullable(optionsProcessor).ifPresent(p -> p.accept(settings, optionEntries));
    }

    private void expandAssertion(StringExpander stringExpander, AssertionEntry assertionEntry) {
        assertionEntry.setName(stringExpander.replaceAllInContent(assertionEntry.getName()));
        assertionEntry.setAssertionClass(stringExpander.replaceAllInContent(assertionEntry.getAssertionClass()));
        assertionEntry.setMessage(stringExpander.replaceAllInContent(assertionEntry.getMessage()));
        expandArray(stringExpander, assertionEntry.getArgs());
    }

    private void expandRequestFile(StringExpander stringExpander, HTTPRequestFileEntry requestFile) {
        requestFile.setName(stringExpander.replaceAllInContent(requestFile.getName()));
        requestFile.setPath(stringExpander.replaceAllInContent(requestFile.getPath()));
        requestFile.setField(stringExpander.replaceAllInContent(requestFile.getField()));
        requestFile.setMineType(stringExpander.replaceAllInContent(requestFile.getMineType()));
    }

    @SuppressWarnings("unchecked")
    private void expandDefaults(StringExpander stringExpander, Map<String, Object> defaults) {
        if(defaults == null) {
            return;
        }
        Map<String, String> map = new HashMap<>();
        for(Map.Entry<String, Object> entry : defaults.entrySet()) {
            if(entry.getValue() instanceof String) {
                String value = stringExpander.replaceAllInContent(map, (String) entry.getValue());
                map.put(entry.getKey(), value);
                defaults.put(entry.getKey(), value);
            } else if(entry.getValue() instanceof List) {
                List<String> values = ((List<String>) entry.getValue())
                    .stream()
                    .map(val -> stringExpander.replaceAllInContent(map, val))
                    .collect(Collectors.toList());
                defaults.put(entry.getKey(), values);
            }
        }
    }

    private void expandMap(StringExpander stringExpander, Map<String, String> map) {
        for(Map.Entry<String, String> entry : map.entrySet()) {
            map.put(entry.getKey(), stringExpander.replaceAllInContent(entry.getValue()));
        }
    }

    private void expandArray(StringExpander stringExpander, String[] array) {
        if(array == null) {
            return;
        }
        for(int i = 0; i < array.length; i++) {
            array[i] = stringExpander.replaceAllInContent(array[i]);
        }
    }

    private void mergeMockDefinition(HTTPMockEntry api, HTTPMockEntry request) {
        if(isBlank(request.getStatusCode())) {
            request.setStatusCode(api.getStatusCode());
        }
        if(isBlank(request.getSecondsDelay())) {
            request.setSecondsDelay(api.getSecondsDelay());
        }

        Map<String, String> responseHeaders = new HashMap<>(api.getResponseHeaders());
        responseHeaders.putAll(request.getResponseHeaders());
        request.setResponseHeaders(responseHeaders);

        if(isBlank(request.getResponseContent())) {
            request.setResponseContent(api.getResponseContent());
        }
        if(isBlank(request.getResponseFilePath())) {
            request.setResponseFilePath(api.getResponseFilePath());
        }
        if(isBlank(request.getExceptionClassOnOutputStream())) {
            request.setExceptionClassOnOutputStream(api.getExceptionClassOnOutputStream());
        }
        if(isBlank(request.getExceptionClassOnResponseCode())) {
            request.setExceptionClassOnResponseCode(api.getExceptionClassOnResponseCode());
        }
    }

    private void mergeRequestFiles(List<HTTPRequestFileEntry> apiFiles, List<HTTPRequestFileEntry> requestFiles) {
        Map<String, HTTPRequestFileEntry> filesByPath = requestFiles
            .stream()
            .collect(Collectors.toMap(HTTPRequestFileEntry::getPath, v -> v));

        for(HTTPRequestFileEntry apiFile : apiFiles) {
            if(filesByPath.get(apiFile.getPath()) == null) {
                requestFiles.add(apiFile);
                continue;
            }

            HTTPRequestFileEntry requestFile = filesByPath.get(apiFile.getPath());
            if(isBlank(requestFile.getName())) {
                requestFile.setName(apiFile.getName());
            }
            if(isBlank(requestFile.getField())) {
                requestFile.setField(apiFile.getField());
            }
            if(isBlank(requestFile.getMineType())) {
                requestFile.setMineType(apiFile.getMineType());
            }
        }
    }
}
