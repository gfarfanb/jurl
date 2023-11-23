package com.legadi.jurl.executor.http;

import static com.legadi.jurl.common.CommonUtils.getOrDefault;
import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.executor.mixer.BodyMixerRegistry.findByBodyType;
import static com.legadi.jurl.options.OptionsRegistry.findByType;
import static com.legadi.jurl.parser.RequestParserRegistry.findByRequestType;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.executor.RequestModifier;
import com.legadi.jurl.executor.mixer.BodyMixer;
import com.legadi.jurl.executor.mixer.BodyMixer.MixerEntry;
import com.legadi.jurl.model.AssertionEntry;
import com.legadi.jurl.model.AuthorizationType;
import com.legadi.jurl.model.RequestInput;
import com.legadi.jurl.model.StepEntry;
import com.legadi.jurl.model.http.HTTPRequestAuthEntry;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;
import com.legadi.jurl.options.OptionsReader.OptionEntry;
import com.legadi.jurl.options.SetInputNameOption;
import com.legadi.jurl.parser.HTTPRequestParser;

public class HTTPRequestModifier implements RequestModifier<HTTPRequestEntry, HTTPResponseEntry> {

    public static final String BODY_TEMPORAL_PATH = "http.request.executor/body.temporal.path";

    private static final Set<String> EXECUTED_AUTH_FLOWS = new HashSet<>();

    @Override
    public String type() {
        return "http";
    }

    @Override
    public Pair<String, RequestInput<?>> appendAuthenticationDefinition(
            Settings settings, RequestInput<HTTPRequestEntry> requestInput) {
        String inputName = isNotBlank(settings.getInputName())
            ? settings.getInputName()
            : requestInput.getDefaultRequest();

        inputName = getOrDefault(inputName,
            requestInput.getRequests().keySet().stream().findFirst().orElse(null));

        if(settings.isSkipAuthentication()) {
            return new Pair<>(inputName, requestInput);
        }

        HTTPRequestEntry request = requestInput.getRequests().get(inputName);

        if(request == null) {
            return new Pair<>(inputName, requestInput);
        }

        HTTPRequestEntry api = requestInput.getApi();

        if(request.getRequestAuth() == null) {
            request.setRequestAuth(api.getRequestAuth());
        } else if(api.getRequestAuth() != null) {
            mergeRequestAuth(api.getRequestAuth(), request.getRequestAuth());
        }

        if(request.getRequestAuth() == null) {
            return new Pair<>(inputName, requestInput);
        }

        HTTPRequestAuthEntry auth = request.getRequestAuth();
        AuthorizationType authType = AuthorizationType.valueOfOrDefault(auth.getAuthType());

        if(isBlank(auth.getRequestInputPath()) && isBlank(auth.getInputName())) {
            return new Pair<>(inputName, requestInput);
        }

        String authFlowName = "flow:auth/" + authType + "+" + inputName;

        if(EXECUTED_AUTH_FLOWS.contains(authFlowName)) {
            return new Pair<>(inputName, requestInput);
        }

        EXECUTED_AUTH_FLOWS.add(authFlowName);
        requestInput.getFlows().put(authFlowName, createAuthAndRequestFlow(inputName, auth));

        return new Pair<>(authFlowName, requestInput);
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

        if(request.getRequestFile() == null) {
            request.setRequestFile(api.getRequestFile());
        } else if(api.getRequestFile() != null) {
            mergeRequestFile(api.getRequestFile(), request.getRequestFile());
        }

        Map<String, String> outputMappings = new LinkedHashMap<>(api.getOutputMappings());
        outputMappings.putAll(request.getOutputMappings());
        request.setOutputMappings(outputMappings);

        List<AssertionEntry> assertions = new LinkedList<>(api.getAssertions());
        assertions.addAll(request.getAssertions());
        request.setAssertions(assertions);
    }

    @Override
    public void mergeBodyFileWithBodyContent(Settings settings, String requestPath, HTTPRequestEntry request) {
        if(isBlank(request.getBodyFilePath())) {
            throw new RequestException(request, "request.bodyFilePath is null or empty");
        }
        if(isBlank(request.getBodyContent())) {
            throw new RequestException(request, "request.bodyContent is null or empty");
        }

        BodyMixer mixer = findByBodyType(settings.getMergeBodyUsingType());
        Path bodyTemporalPath = mixer.apply(settings, new MixerEntry()
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
        HTTPRequestParser parser = findByRequestType(type());
        HTTPRequestEntry overrideRequest = parser.parseRequest(settings, Paths.get(filename));

        if(isNotEmpty(overrideRequest.getHeaders())) {
            request.setHeaders(overrideRequest.getHeaders());
        }

        if(isNotEmpty(overrideRequest.getQueryParams())) {
            request.setQueryParams(overrideRequest.getQueryParams());
        }

        if(isNotEmpty(overrideRequest.getAssertions())) {
            request.setAssertions(overrideRequest.getAssertions());
        }

        if(isNotEmpty(overrideRequest.getOutputMappings())) {
            request.setOutputMappings(overrideRequest.getOutputMappings());
        }

        if(isNotBlank(overrideRequest.getBodyContent())) {
            request.setBodyContent(overrideRequest.getBodyContent());
        }

        if(isNotBlank(overrideRequest.getBodyFilePath())) {
            request.setBodyFilePath(overrideRequest.getBodyFilePath());
        }
    }

    private List<StepEntry> createAuthAndRequestFlow(String inputName, HTTPRequestAuthEntry auth) {
        List<StepEntry> steps = new LinkedList<>();

        StepEntry authStep = new StepEntry();
        
        if(isNotBlank(auth.getRequestInputPath())) {
            authStep.setRequestInputPath(auth.getRequestInputPath());
        }

        if(isNotBlank(auth.getInputName())) {
            List<OptionEntry> authOptions = new LinkedList<>();

            authOptions.add(new OptionEntry(findByType(SetInputNameOption.class),
                new String[] { auth.getInputName() }));
            authStep.setOptions(authOptions);
        }

        steps.add(authStep);

        StepEntry requestStep = new StepEntry();
        List<OptionEntry> requestOptions = new LinkedList<>();

        requestOptions.add(new OptionEntry(findByType(SetInputNameOption.class),
            new String[] { inputName }));
        requestStep.setOptions(requestOptions);

        steps.add(requestStep);

        return steps;
    }

    private void mergeRequestFile(HTTPRequestFileEntry api, HTTPRequestFileEntry request) {
        if(isBlank(request.getName())) {
            request.setName(api.getName());
        }
        if(isBlank(request.getPath())) {
            request.setPath(api.getPath());
        }
        if(isBlank(request.getField())) {
            request.setField(api.getField());
        }
        if(isBlank(request.getMineType())) {
            request.setMineType(api.getMineType());
        }

        Map<String, String> formData = new HashMap<>(api.getFormData());
        formData.putAll(request.getFormData());
        request.setFormData(formData);
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
}
