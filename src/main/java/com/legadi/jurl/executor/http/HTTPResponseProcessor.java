package com.legadi.jurl.executor.http;

import static com.legadi.jurl.assertions.AssertionsResolver.evaluate;
import static com.legadi.jurl.common.CommonUtils.isEmpty;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.jurl.common.JsonUtils.writeJsonFile;
import static com.legadi.jurl.common.WriterUtils.printFile;
import static com.legadi.jurl.executor.reader.OutputReaderRegistry.findByContentType;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.common.StringExpander;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.executor.ResponseProcessor;
import com.legadi.jurl.executor.reader.OutputReader;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.RequestBehaviour;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class HTTPResponseProcessor implements ResponseProcessor<HTTPRequestEntry, HTTPResponseEntry> {

    private static final Logger LOGGER = Logger.getLogger(HTTPResponseProcessor.class.getName());

    private static final String HTTP_PREFIX = "HTTP/";
    private static final String OUTPUT_PREFIX = "OUT/";

    @Override
    public Optional<AssertionResult> processResponse(Settings settings, HTTPRequestEntry request, HTTPResponseEntry response)
            throws RequestException {
        RequestBehaviour behaviour = settings.getRequestBehaviour();

        switch(behaviour) {
            case CURL_ONLY:
                LOGGER.info(response.getCurlCommand());
                return Optional.empty();
            case PRINT_ONLY:
                return Optional.empty();
            default:
                break;
        }

        mapOutput(settings, response);

        StringExpander stringExpander = new StringExpander(settings);
        Set<String> outputParams = scanOutputParams(stringExpander, request);
        Map<String, String> values = readOutputValues(settings, response, outputParams);

        saveOutput(stringExpander, values, request, response);

        if(settings.isSkipAssertions()) {
            return Optional.empty();
        } else {
            return evaluate(settings, values, request.getAssertions());
        }
    }

    private void mapOutput(Settings settings, HTTPResponseEntry response) {
        settings.putOverride(HTTP_PREFIX + "url", response.getRequestUrl());
        settings.putOverride(HTTP_PREFIX + "curl", response.getCurlCommand());
        settings.putOverride(HTTP_PREFIX + "status", Integer.toString(response.getStatusCode()));

        if(response.getResponsePath() != null) {
            settings.putOverride(HTTP_PREFIX + "response.path", response.getResponsePath().toString());
        }

        if(isNotEmpty(response.getResponseHeaders())) {
            for(Map.Entry<String, String> headerEntry : response.getResponseHeaders().entrySet()) {
                settings.putOverride(HTTP_PREFIX + "header." + headerEntry.getKey(), headerEntry.getValue());
            }
        }
    }

    private Set<String> scanOutputParams(StringExpander stringExpander, HTTPRequestEntry request) {
        Set<String> outputParams = new HashSet<>();

        if(isNotEmpty(request.getOutputMappings())) {
            outputParams.addAll(
                request.getOutputMappings().entrySet()
                    .parallelStream()
                    .map(Map.Entry::getValue)
                    .map(stringExpander::scanParamsInContent)
                    .flatMap(Set::stream)
                    .filter(param -> param.startsWith(OUTPUT_PREFIX))
                    .collect(Collectors.toSet())
            );
        }

        if(isNotEmpty(request.getAssertions())) {
            outputParams.addAll(
                request.getAssertions()
                    .parallelStream()
                    .map(assertion -> {
                        List<String> elements = new LinkedList<>();
                        elements.add(assertion.getAssertionClass());
                        elements.add(assertion.getMessage());
                        elements.addAll(Arrays.asList(assertion.getArgs()));
                        return elements;
                    })
                    .flatMap(List::stream)
                    .filter(Objects::nonNull)
                    .map(stringExpander::scanParamsInContent)
                    .flatMap(Set::stream)
                    .filter(param -> param.startsWith(OUTPUT_PREFIX))
                    .collect(Collectors.toSet())
            );
        }

        return outputParams;
    }

    private void saveOutput(StringExpander stringExpander, Map<String, String> values,
            HTTPRequestEntry request, HTTPResponseEntry response) {
        if(isEmpty(request.getOutputMappings())) {
            return;
        }

        Map<String, String> outputProperties = new HashMap<>();
        StringBuilder printableOutput = new StringBuilder();

        request.getOutputMappings().forEach((key, value) -> {
            value = stringExpander.replaceAllInContent(values, value);

            values.put(key, value);
            outputProperties.put(key, value);
            printableOutput
                .append("\n")
                .append(key)
                .append(" <- ")
                .append(value);
        });

        Settings.mergeProperties(stringExpander.getSettings().getEnvironment(), outputProperties);

        Path overridePath = stringExpander.getSettings().getOverrideFilePath();
        Map<String, String> overrideProperties = loadJsonProperties(overridePath);

        overrideProperties.putAll(outputProperties);

        writeJsonFile(overridePath, overrideProperties);

        LOGGER.info("Output saved [" + stringExpander.getSettings().getEnvironment() + "]: " + overridePath);
        LOGGER.info(printableOutput.toString());
    }

    private Map<String, String> readOutputValues(Settings settings, HTTPResponseEntry response,
            Set<String> outputParams) {
        if(response.getResponsePath() == null) {
            return new HashMap<>();
        }

        String contentType = response.getResponseHeaders()
            .entrySet()
            .stream()
            .filter(e -> "Content-Type".equalsIgnoreCase(e.getKey()))
            .map(Map.Entry::getValue)
            .findFirst()
            .orElse("");
        Optional<OutputReader> outputReader = findByContentType(contentType);
        Map<String, String> outputValues;
        boolean isPrintable;

        if(outputReader.isPresent()) {
            isPrintable = outputReader.get().isPrintable();
            outputValues = outputReader.get().apply(response.getResponsePath(),
                settings.getOutputObjectPath(), outputParams, OUTPUT_PREFIX);
        } else {
            isPrintable = Arrays.stream(settings.getPrintableMimeTypes())
                .anyMatch(type -> type.equalsIgnoreCase(contentType));
            outputValues = new HashMap<>();
        }

        if(isPrintable) {
            printFile(response.getResponsePath());
        }

        if(isNotEmpty(outputValues)) {
            StringBuilder printableOutput = new StringBuilder();

            outputValues.forEach((output, value) -> printableOutput
                .append("\n")
                .append(output)
                .append(" <- ")
                .append(value));

            LOGGER.info("Processed output [" + contentType + "]: " + response.getResponsePath());
            LOGGER.info(printableOutput.toString());
        }

        return outputValues;
    }
}
