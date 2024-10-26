package com.legadi.cli.jurl.executor.http;

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
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
        Map<String, Object> defaults = new HashMap<>(api.getDefaults());
        defaults.putAll(request.getDefaults());

        expandRequestAuth(settings, auth, defaults);

        requestInputPath = isNotBlank(auth.getRequestInputPath())
            ? auth.getRequestInputPath() : requestInputPath;

        RequestParser<HTTPRequestEntry> requestParser = findOrFail(RequestParser.class, settings.getRequestType());
        RequestInput<HTTPRequestEntry> authRequestInput = requestParser.parseInput(settings, Paths.get(requestInputPath));

        if(isEmpty(authRequestInput.getRequests())) {
            return Optional.empty();
        }

        InputNameResolver inputNameResolver = new InputNameResolver(settings,
            requestInputPath, authRequestInput);
        String authRequestName = inputNameResolver.resolve(auth.getInputName());
        HTTPRequestEntry authRequestEntry = authRequestInput.getRequests().get(authRequestName);

        if(authRequestEntry == null) {
            return Optional.empty();
        }

        AuthorizationType authType = AuthorizationType.valueOfOrDefault(auth.getAuthType());
        AuthenticationRequest<HTTPRequestEntry> authRequest = new AuthenticationRequest<>();

        List<OptionEntry> authOptions = new ArrayList<>(options);
        authOptions.addAll(api.getOptions());

        authRequestEntry.getDefaults().putAll(defaults);

        authRequest.setAuthRequestInputPath(requestInputPath);
        authRequest.setAuthRequestName(authRequestName);
        authRequest.setAuthType(authType);
        authRequest.setAuthOptions(authOptions
                .stream()
                .filter(opt -> opt.getLeft().allowedForRequestAuth())
                .collect(Collectors.toList()));
        authRequest.setAuthApi(authRequestInput.getApi());
        authRequest.setAuthRequest(authRequestEntry);

        StringExpander stringExpander = new StringExpander(settings, defaults);
        authRequest.getAuthOptions().stream().map(Pair::getRight)
            .forEach(args -> expandArray(stringExpander, args, filterDefaults(defaults)));

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

        Map<String, Object> defaults = new HashMap<>(api.getDefaults());
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
        StringExpander stringExpander = new StringExpander(settings, flow.getDefaults());

        expandDefaults(stringExpander, flow.getDefaults());

        Map<String, String> defaults = filterDefaults(flow.getDefaults());

        for(StepEntry step : flow.getSteps()) {
            step.setRequestInputPath(stringExpander.replaceAllInContent(defaults, step.getRequestInputPath()));
            step.getOptions().stream().map(OptionEntry::getRight)
                .forEach(args -> expandArray(stringExpander, args, defaults));
        }
    }

    @Override
    public void expandRequestDefinition(Settings settings, HTTPRequestEntry request) {
        StringExpander stringExpander = new StringExpander(settings, request.getDefaults());

        expandDefaults(stringExpander, request.getDefaults());

        Map<String, String> defaults = filterDefaults(request.getDefaults());

        request.setUrl(stringExpander.replaceAllInContent(defaults, request.getUrl()));
        request.setProtocol(stringExpander.replaceAllInContent(defaults, request.getProtocol()));
        request.setHost(stringExpander.replaceAllInContent(defaults, request.getHost()));
        request.setPort(stringExpander.replaceAllInContent(defaults, request.getPort()));
        request.setBasePath(stringExpander.replaceAllInContent(defaults, request.getBasePath()));
        request.setEndpoint(stringExpander.replaceAllInContent(defaults, request.getEndpoint()));

        if(request.getMockDefinition() != null) {
            HTTPMockEntry mock = request.getMockDefinition();

            expandMap(stringExpander, mock.getResponseHeaders(), defaults);
            mock.setResponseContent(stringExpander.replaceAllInContent(defaults, mock.getResponseContent()));
            mock.setResponseFilePath(stringExpander.replaceAllInContent(defaults, mock.getResponseFilePath()));
            mock.setExceptionClassOnOutputStream(stringExpander.replaceAllInContent(defaults, mock.getExceptionClassOnOutputStream()));
            mock.setExceptionClassOnResponseCode(stringExpander.replaceAllInContent(defaults, mock.getExceptionClassOnResponseCode()));
        }

        request.getConditions().forEach(condition -> expandAssertion(stringExpander, condition, defaults));
        request.getOptions().stream().map(Pair::getRight).forEach(args -> expandArray(stringExpander, args, defaults));

        request.setMethod(stringExpander.replaceAllInContent(defaults, request.getMethod()));
        expandMap(stringExpander, request.getQueryParams(), defaults);
        expandMap(stringExpander, request.getHeaders(), defaults);
        request.setBodyCharset(stringExpander.replaceAllInContent(defaults, request.getBodyCharset()));
        request.setBodyContent(stringExpander.replaceAllInContent(defaults, request.getBodyContent()));
        request.setBodyFilePath(stringExpander.replaceAllInContent(defaults, request.getBodyFilePath()));
        request.getRequestFiles().forEach(requestFile -> expandRequestFile(stringExpander, requestFile, defaults));
        expandMap(stringExpander, request.getFormData(), defaults);
    }

    private void expandRequestAuth(Settings settings, HTTPRequestAuthEntry requestAuth,
            Map<String, Object> authDefaults) {
        StringExpander stringExpander = new StringExpander(settings, authDefaults);

        expandDefaults(stringExpander, authDefaults);

        Map<String, String> defaults = filterDefaults(authDefaults);

        requestAuth.setRequestInputPath(stringExpander.replaceAllInContent(defaults, requestAuth.getRequestInputPath()));
        requestAuth.setInputName(stringExpander.replaceAllInContent(defaults, requestAuth.getInputName()));
        requestAuth.setAuthType(stringExpander.replaceAllInContent(defaults, requestAuth.getAuthType()));
        requestAuth.setTokenParam(stringExpander.replaceAllInContent(defaults, requestAuth.getTokenParam()));
        requestAuth.setUsernameParam(stringExpander.replaceAllInContent(defaults, requestAuth.getUsernameParam()));
        requestAuth.setPasswordParam(stringExpander.replaceAllInContent(defaults, requestAuth.getPasswordParam()));
    }

    private void expandAssertion(StringExpander stringExpander, AssertionEntry assertionEntry,
            Map<String, String> defaults) {
        assertionEntry.setName(stringExpander.replaceAllInContent(defaults, assertionEntry.getName()));
        assertionEntry.setAssertionClass(stringExpander.replaceAllInContent(defaults, assertionEntry.getAssertionClass()));
        assertionEntry.setMessage(stringExpander.replaceAllInContent(defaults, assertionEntry.getMessage()));
        expandArray(stringExpander, assertionEntry.getArgs(), defaults);
    }

    private void expandRequestFile(StringExpander stringExpander, HTTPRequestFileEntry requestFile,
            Map<String, String> defaults) {
        requestFile.setName(stringExpander.replaceAllInContent(defaults, requestFile.getName()));
        requestFile.setPath(stringExpander.replaceAllInContent(defaults, requestFile.getPath()));
        requestFile.setField(stringExpander.replaceAllInContent(defaults, requestFile.getField()));
        requestFile.setMineType(stringExpander.replaceAllInContent(defaults, requestFile.getMineType()));
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

    private void expandMap(StringExpander stringExpander, Map<String, String> map,
            Map<String, String> defaults) {
        for(Map.Entry<String, String> entry : map.entrySet()) {
            map.put(entry.getKey(), stringExpander.replaceAllInContent(defaults, entry.getValue()));
        }
    }

    private void expandArray(StringExpander stringExpander, String[] array,
            Map<String, String> defaults) {
        if(array == null) {
            return;
        }
        for(int i = 0; i < array.length; i++) {
            array[i] = stringExpander.replaceAllInContent(defaults, array[i]);
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

    private Map<String, String> filterDefaults(Map<String, Object> defaults) {
        return Optional.ofNullable(defaults)
            .map(Map::entrySet)
            .map(Set::stream)
            .orElse(Stream.empty())
            .filter(e -> e.getValue() instanceof String)
            .collect(Collectors.toMap(Map.Entry::getKey, e -> (String) e.getValue()));
    }
}
