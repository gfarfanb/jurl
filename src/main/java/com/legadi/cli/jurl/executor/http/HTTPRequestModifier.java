package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.CommonUtils.INPUT_DEFAULT_FORMAT;
import static com.legadi.cli.jurl.common.CommonUtils.INPUT_FORMAT;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isEmpty;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findOrFail;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.legadi.cli.jurl.common.CommonUtils;
import com.legadi.cli.jurl.common.InputNameResolver;
import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.StringExpander;
import com.legadi.cli.jurl.exception.RequestException;
import com.legadi.cli.jurl.executor.RequestModifier;
import com.legadi.cli.jurl.executor.mixer.BodyMixer;
import com.legadi.cli.jurl.executor.mixer.BodyMixer.MixerEntry;
import com.legadi.cli.jurl.model.AssertionEntry;
import com.legadi.cli.jurl.model.AuthenticationRequest;
import com.legadi.cli.jurl.model.AuthorizationType;
import com.legadi.cli.jurl.model.FlowEntry;
import com.legadi.cli.jurl.model.RequestInput;
import com.legadi.cli.jurl.model.StepEntry;
import com.legadi.cli.jurl.model.http.HTTPMockEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestAuthEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;
import com.legadi.cli.jurl.options.OptionsReader.OptionEntry;
import com.legadi.cli.jurl.parser.HTTPRequestParser;
import com.legadi.cli.jurl.parser.RequestParser;

public class HTTPRequestModifier implements RequestModifier<HTTPRequestEntry, HTTPResponseEntry> {

    public static final String BODY_TEMPORAL_PATH = "http.request.executor/body.temporal.path";

    @Override
    public String name() {
        return "http";
    }

    @Override
    public Optional<AuthenticationRequest<HTTPRequestEntry>> getAuthenticationDefinition(
            String requestName, String requestInputPath, RequestInput<HTTPRequestEntry> requestInput,
            Settings settings, List<OptionEntry> options) {
        HTTPRequestEntry request = requestInput.getRequests().get(requestName);
        HTTPRequestEntry api = requestInput.getApi();

        if(request.getRequestAuth() == null) {
            request.setRequestAuth(api.getRequestAuth());
        } else if(api.getRequestAuth() != null) {
            mergeRequestAuth(api.getRequestAuth(), request.getRequestAuth());
        }

        if(request.getRequestAuth() == null) {
            return Optional.empty();
        }

        HTTPRequestAuthEntry auth = request.getRequestAuth();
        Map<String, String> defaults = new HashMap<>(api.getDefaults());
        defaults.putAll(request.getDefaults());

        expandRequestAuth(settings, auth, defaults);

        requestInputPath = isNotBlank(auth.getRequestInputPath())
            ? auth.getRequestInputPath() : requestInputPath;

        RequestParser<HTTPRequestEntry> requestParser = findOrFail(RequestParser.class, settings.getRequestType());
        RequestInput<HTTPRequestEntry> authRequestInput = requestParser.parseInput(settings, Paths.get(requestInputPath));

        if(isEmpty(authRequestInput.getRequests())) {
            return Optional.empty();
        }

        InputNameResolver inputNameResolver = new InputNameResolver(requestInputPath, authRequestInput);
        String authRequestName = inputNameResolver.resolve(auth.getInputName());
        HTTPRequestEntry authRequestEntry = authRequestInput.getRequests().get(authRequestName);

        if(authRequestEntry == null) {
            return Optional.empty();
        }

        AuthorizationType authType = AuthorizationType.valueOfOrDefault(auth.getAuthType());
        AuthenticationRequest<HTTPRequestEntry> authRequest = new AuthenticationRequest<>();

        authRequest.setAuthRequestInputPath(requestInputPath);
        authRequest.setAuthRequestName(authRequestName);
        authRequest.setAuthType(authType);
        authRequest.setAuthOptions(options
                .stream()
                .filter(opt -> opt.getLeft().allowedForRequestAuth())
                .collect(Collectors.toList()));
        authRequest.setAuthApi(authRequestInput.getApi());
        authRequest.setAuthRequest(authRequestEntry);

        return Optional.of(authRequest);
    }

    @Override
    public void mergeRequestHeader(HTTPRequestEntry api, HTTPRequestEntry request) {
        if(isBlank(request.getUrl())) {
            request.setUrl(api.getUrl());
        }
        if(isBlank(request.getProtocol())) {
            request.setProtocol(api.getProtocol());
        }
        if(isBlank(request.getHost())) {
            request.setHost(api.getHost());
        }
        if(isBlank(request.getPort())) {
            request.setPort(api.getPort());
        }
        if(isBlank(request.getBasePath())) {
            request.setBasePath(api.getBasePath());
        }
        if(isBlank(request.getEndpoint())) {
            request.setEndpoint(api.getEndpoint());
        }
    }

    @Override
    public void mergeAPIDefinition(Settings settings, HTTPRequestEntry api, HTTPRequestEntry request) {
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

        mergeRequestFiles(api.getRequestFiles(), request.getRequestFiles());

        Map<String, String> formData = new HashMap<>(api.getFormData());
        formData.putAll(request.getFormData());
        request.setFormData(formData);

        Map<String, String> defaults = new HashMap<>(api.getDefaults());
        defaults.putAll(request.getDefaults());
        request.setDefaults(defaults);
    }

    @Override
    public void mergeBodyFileWithBodyContent(Settings settings, String requestPath, HTTPRequestEntry request) {
        if(isBlank(request.getBodyFilePath())) {
            throw new RequestException(request, "request.bodyFilePath is null or empty");
        }
        if(isBlank(request.getBodyContent())) {
            throw new RequestException(request, "request.bodyContent is null or empty");
        }

        BodyMixer mixer = findOrFail(BodyMixer.class, settings.getMergeBodyUsingType());
        Path bodyTemporalPath = mixer.apply(settings, request.getDefaults(),
            new MixerEntry()
                .setRequestPath(requestPath)
                .setRequestName(request.getName())
                .setBodyFilePath(request.getBodyFilePath())
                .setBodyContent(request.getBodyContent()));

        request.setBodyContent(null);
        request.setBodyFilePath(null);

        settings.putOverride(BODY_TEMPORAL_PATH, bodyTemporalPath.toString());
    }

    @Override
    public void overrideRequestWithFile(Settings settings, HTTPRequestEntry request, String filename) {
        HTTPRequestParser parser = findOrFail(RequestParser.class, name());
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
    }

    @Override
    public void expandFlow(Settings settings, FlowEntry flow) {
        StringExpander stringExpander = new StringExpander(settings,
            new PropertyDefault(flow.getDefaults()));

        expandMap(stringExpander, flow.getDefaults(), true);

        for(StepEntry step : flow.getSteps()) {
            step.setRequestInputPath(stringExpander.replaceAllInContent(step.getRequestInputPath()));
            step.getOptions().stream().map(OptionEntry::getRight).forEach(args -> expandArray(stringExpander, args));
        }
    }

    @Override
    public void expandRequestDefinition(Settings settings, HTTPRequestEntry request) {
        StringExpander stringExpander = new StringExpander(settings,
            new PropertyDefault(request.getDefaults()));

        expandMap(stringExpander, request.getDefaults(), true);

        request.setUrl(stringExpander.replaceAllInContent(request.getUrl()));
        request.setProtocol(stringExpander.replaceAllInContent(request.getProtocol()));
        request.setHost(stringExpander.replaceAllInContent(request.getHost()));
        request.setPort(stringExpander.replaceAllInContent(request.getPort()));
        request.setBasePath(stringExpander.replaceAllInContent(request.getBasePath()));
        request.setEndpoint(stringExpander.replaceAllInContent(request.getEndpoint()));

        if(request.getMockDefinition() != null) {
            HTTPMockEntry mock = request.getMockDefinition();

            expandMap(stringExpander, mock.getResponseHeaders(), false);
            mock.setResponseContent(stringExpander.replaceAllInContent(mock.getResponseContent()));
            mock.setResponseFilePath(stringExpander.replaceAllInContent(mock.getResponseFilePath()));
            mock.setExceptionClassOnOutputStream(stringExpander.replaceAllInContent(mock.getExceptionClassOnOutputStream()));
            mock.setExceptionClassOnResponseCode(stringExpander.replaceAllInContent(mock.getExceptionClassOnResponseCode()));
        }

        request.getConditions().forEach(condition -> expandAssertion(stringExpander, condition));
        request.getOptions().stream().map(Pair::getRight).forEach(args -> expandArray(stringExpander, args));

        request.setMethod(stringExpander.replaceAllInContent(request.getMethod()));
        expandMap(stringExpander, request.getQueryParams(), false);
        expandMap(stringExpander, request.getHeaders(), false);
        request.setBodyCharset(stringExpander.replaceAllInContent(request.getBodyCharset()));
        request.setBodyContent(stringExpander.replaceAllInContent(request.getBodyContent()));
        request.setBodyFilePath(stringExpander.replaceAllInContent(request.getBodyFilePath()));
        request.getRequestFiles().forEach(requestFile -> expandRequestFile(stringExpander, requestFile));
        expandMap(stringExpander, request.getFormData(), false);
    }

    private void expandRequestAuth(Settings settings, HTTPRequestAuthEntry requestAuth,
            Map<String, String> defaults) {
        StringExpander stringExpander = new StringExpander(settings,
            new PropertyDefault(defaults));

        expandMap(stringExpander, defaults, true);

        requestAuth.setRequestInputPath(stringExpander.replaceAllInContent(requestAuth.getRequestInputPath()));
        requestAuth.setInputName(stringExpander.replaceAllInContent(requestAuth.getInputName()));
        requestAuth.setAuthType(stringExpander.replaceAllInContent(requestAuth.getAuthType()));
        requestAuth.setTokenParam(stringExpander.replaceAllInContent(requestAuth.getTokenParam()));
        requestAuth.setUsernameParam(stringExpander.replaceAllInContent(requestAuth.getUsernameParam()));
        requestAuth.setPasswordParam(stringExpander.replaceAllInContent(requestAuth.getPasswordParam()));
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

    private void expandMap(StringExpander stringExpander, Map<String, String> map, boolean isDefaults) {
        if(map == null) {
            return;
        }
        for(Map.Entry<String, String> entry : map.entrySet()) {
            if(isDefaults) {
                map.put(entry.getKey(), stringExpander.replaceAllInContent(map, entry.getValue()));
            } else {
                map.put(entry.getKey(), stringExpander.replaceAllInContent(entry.getValue()));
            }
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
            if(!filesByPath.containsKey(apiFile.getPath())) {
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

    private void mergeRequestAuth(HTTPRequestAuthEntry api, HTTPRequestAuthEntry request) {
        if(isBlank(request.getRequestInputPath())) {
            request.setRequestInputPath(api.getRequestInputPath());
        }
        if(isBlank(request.getInputName())) {
            request.setInputName(api.getInputName());
        }
        if(isBlank(request.getAuthType())) {
            request.setAuthType(api.getAuthType());
        }
        if(isBlank(request.getTokenParam())) {
            request.setTokenParam(api.getTokenParam());
        }
        if(isBlank(request.getUsernameParam())) {
            request.setUsernameParam(api.getUsernameParam());
        }
        if(isBlank(request.getPasswordParam())) {
            request.setPasswordParam(api.getPasswordParam());
        }
    }

    public static class PropertyDefault implements Function<String, String> {

        private final Map<String, String> defaults;

        public PropertyDefault(Map<String, String> defaults) {
            this.defaults = defaults;
        }

        @Override
        public String apply(String property) {
            String defaultValue = defaults.get(property);
            String message = defaultValue != null
                ? String.format(INPUT_DEFAULT_FORMAT, property, defaultValue)
                : String.format(INPUT_FORMAT, property);

            if(defaultValue == null) {
                defaultValue = "";
            }

            return Optional.ofNullable(System.console())
                .map(console -> console.readLine(message))
                .filter(CommonUtils::isNotBlank)
                .orElse(defaultValue);
        }
    }
}
