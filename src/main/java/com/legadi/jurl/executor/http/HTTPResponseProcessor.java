package com.legadi.jurl.executor.http;

import static com.legadi.jurl.assertions.AssertionsResolver.evaluate;
import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isEmpty;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.jurl.common.JsonUtils.writeJsonFile;
import static com.legadi.jurl.common.WriterUtils.printFile;
import static com.legadi.jurl.executor.decoder.OutputDecoderRegistry.findByContentEncoding;
import static com.legadi.jurl.executor.reader.OutputReaderRegistry.findByContentType;
import static java.util.logging.Level.FINE;

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
import com.legadi.jurl.executor.decoder.OutputDecoder;
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
    public String type() {
        return "http";
    }

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

        Path responsePath = unzipResponse(response);

        mapOutput(settings, response, responsePath);

        StringExpander stringExpander = new StringExpander(settings);
        Set<String> outputParams = scanOutputParams(stringExpander, request);
        Map<String, String> values = null;

        try {
            values = readOutputValues(settings, response, outputParams, responsePath);
            saveOutput(stringExpander, values, request, response);
        } catch(Exception ex) {
            LOGGER.warning("Unable to read response [" + responsePath + "] - " + ex.getMessage());
            LOGGER.log(FINE, ex.getMessage(), ex);
            values = new HashMap<>();
        }

        if(settings.isSkipAssertions()) {
            return Optional.empty();
        } else {
            return evaluate(settings, values, request.getAssertions());
        }
    }

    @Override
    public Map<String, Object> getDetailsFromResponse(HTTPResponseEntry response) {
        if(response == null) {
            return null;
        }

        Map<String, Object> details = new HashMap<>();

        if(response.getBodyPath() != null) {
            details.put("bodyPath", response.getBodyPath().toString());
        }
        if(response.getSentFilePath() != null) {
            details.put("sentFilePath", response.getSentFilePath().toString());
        }
        if(response.getResponsePath() != null) {
            details.put("responsePath", response.getResponsePath().toString());
        }
        if(response.getStatusCode() > 0) {
            details.put("statusCode", response.getStatusCode());
        }
        if(isNotEmpty(response.getResponseHeaders())) {
            details.put("responseHeaders", response.getResponseHeaders());
        }

        return details;
    }

    private void mapOutput(Settings settings, HTTPResponseEntry response, Path responsePath) {
        settings.putOverride(HTTP_PREFIX + "url", response.getRequestUrl());
        settings.putOverride(HTTP_PREFIX + "curl", response.getCurlCommand());
        settings.putOverride(HTTP_PREFIX + "status", Integer.toString(response.getStatusCode()));

        if(responsePath != null) {
            settings.putOverride(HTTP_PREFIX + "response.path", responsePath.toString());
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

        LOGGER.fine("");
        LOGGER.fine("Output saved [" + stringExpander.getSettings().getEnvironment() + "]: " + overridePath);
        LOGGER.fine(printableOutput.toString());
    }

    private Path unzipResponse(HTTPResponseEntry response) {
        String contentEncoding = response.getResponseHeaders()
            .entrySet()
            .stream()
            .filter(e -> "Content-Encoding".equalsIgnoreCase(e.getKey()))
            .map(Map.Entry::getValue)
            .findFirst()
            .orElse("");

        if(isBlank(contentEncoding)) {
            return response.getResponsePath();
        }

        Optional<OutputDecoder> outputDecoder = findByContentEncoding(contentEncoding);

        if(outputDecoder.isPresent()) {
            return outputDecoder.get().apply(response.getResponsePath());
        } else {
            return response.getResponsePath();
        }
    }

    private Map<String, String> readOutputValues(Settings settings, HTTPResponseEntry response,
            Set<String> outputParams, Path responsePath) {
        if(responsePath == null) {
            return new HashMap<>();
        }

        String contentType = response.getResponseHeaders()
            .entrySet()
            .stream()
            .filter(e -> "Content-Type".equalsIgnoreCase(e.getKey()))
            .map(Map.Entry::getValue)
            .findFirst()
            .orElse("");
        Optional<OutputReader> outputReader;

        if(isNotBlank(contentType)) {
            outputReader = findByContentType(contentType);
        } else {
            outputReader = Optional.empty();
        }

        Map<String, String> outputValues;
        boolean isPrintable;

        if(outputReader.isPresent()) {
            isPrintable = outputReader.get().isPrintable();
            outputValues = outputReader.get().apply(responsePath,
                settings.getOutputObjectPath(), outputParams, OUTPUT_PREFIX);
        } else {
            isPrintable = Arrays.stream(settings.getPrintableMimeTypes())
                .anyMatch(type -> type.equalsIgnoreCase(contentType));
            outputValues = new HashMap<>();
        }

        if(isPrintable) {
            printFile(responsePath);
        }

        if(isNotEmpty(outputValues)) {
            StringBuilder printableOutput = new StringBuilder();

            outputValues.forEach((output, value) -> printableOutput
                .append("\n")
                .append(output)
                .append(" <- ")
                .append(value));

            LOGGER.fine("");
            LOGGER.fine("Processed output [" + contentType + "]: " + responsePath);
            LOGGER.fine(printableOutput.toString());
        }

        return outputValues;
    }
}
