package com.legadi.jurl.executor;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.legadi.jurl.assertions.AssertionFunction;
import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.AssertException;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.executor.reader.JsonOutputReader;
import com.legadi.jurl.executor.reader.XmlOutputReader;
import com.legadi.jurl.model.AssertionEntry;
import com.legadi.jurl.model.HTTPRequestEntry;
import com.legadi.jurl.model.HTTPResponseEntry;
import com.legadi.jurl.model.OutputType;

import static com.legadi.jurl.assertions.AssertionsRegistry.findByName;
import static com.legadi.jurl.assertions.AssertionsRegistry.registerAssertFunction;
import static com.legadi.jurl.common.LoaderUtils.loadJsonProperties;
import static com.legadi.jurl.common.StringUtils.replaceAllInContent;
import static com.legadi.jurl.common.StringUtils.scanParamsInContent;
import static com.legadi.jurl.common.WriterUtils.writeJsonFile;

public class HTTPResponseProcessor implements ResponseProcessor<HTTPRequestEntry, HTTPResponseEntry> {

    private static final Logger LOGGER = Logger.getLogger(HTTPResponseProcessor.class.getName());

    private static final String HTTP_PREFIX = "HTTP:";
    private static final String OUTPUT_PREFIX = "OUT:";

    @Override
    public void processResponse(Settings settings, HTTPRequestEntry request, HTTPResponseEntry response,
            long nanoElapsed) throws RequestException {

        if(settings.isCurlRequest()) {
            LOGGER.info(response.getCurlCommand());
            return;
        }

        mapOutput(settings, response);
        saveOutput(settings, request, response);
        evaluate(settings, request.getAssertions());
    }

    private void mapOutput(Settings settings, HTTPResponseEntry response) {
        settings.putOverride(HTTP_PREFIX + "url", response.getRequestUrl());
        settings.putOverride(HTTP_PREFIX + "curl", response.getCurlCommand());
        settings.putOverride(HTTP_PREFIX + "status", Integer.toString(response.getStatusCode()));

        if(response.getResponsePath() != null) {
            settings.putOverride(HTTP_PREFIX + "response.path", response.getResponsePath().toString());
        }

        if(response.getResponseHeaders() !=null) {
            for(Map.Entry<String, String> headerEntry : response.getResponseHeaders().entrySet()) {
                settings.putOverride(HTTP_PREFIX + "header." + headerEntry.getKey(), headerEntry.getValue());
            }
        }
    }

    private void saveOutput(Settings settings, HTTPRequestEntry request, HTTPResponseEntry response) {
        if(request.getOutputMappings() == null) {
            return;
        }

        Set<String> outputParams = request.getOutputMappings().entrySet()
            .parallelStream()
            .map(Map.Entry::getValue)
            .map(value -> scanParamsInContent(settings, value))
            .flatMap(Set::stream)
            .filter(param -> param.startsWith(OUTPUT_PREFIX))
            .collect(Collectors.toSet());

        Map<String, String> values = readOutputValues(request.getOutputType(), response, outputParams);
        Map<String, String> outputProperties = request.getOutputMappings().entrySet()
            .parallelStream()
            .map(e -> new Pair<>(
                e.getKey(),
                replaceAllInContent(settings, values, e.getValue())
            ))
            .collect(Collectors.toMap(
                Pair::getLeft,
                Pair::getRight,
                (v1, v2) -> v2,
                HashMap::new
            ));

        Settings.mergeProperties(settings.getEnvironment(), outputProperties);

        String overrideFile = settings.getOverrideFileName();
        Map<String, String> overrideProperties = loadJsonProperties(overrideFile, true);

        overrideProperties.putAll(outputProperties);

        writeJsonFile(overrideFile, overrideProperties);
    }

    private Map<String, String> readOutputValues(OutputType outputType, HTTPResponseEntry response,
            Set<String> outputParams) {
        if(response.getResponsePath() == null) {
            return new HashMap<>();
        }

        LOGGER.info("Processing output [" + outputType + "]: " + response.getResponsePath());

        switch(outputType) {
            case JSON:
                return new JsonOutputReader().apply(response.getResponsePath(), outputParams, OUTPUT_PREFIX);
            case XML:
                return new XmlOutputReader().apply(response.getResponsePath(), outputParams, OUTPUT_PREFIX);
            default:
                if(response.getResponsePath() != null) {
                    LOGGER.info("Generated output [" + outputType + "]: " + response.getResponsePath());
                }
                return new HashMap<>();
        }
    }

    private void evaluate(Settings settings, List<AssertionEntry> assertions) {
        if(assertions == null || settings.isSkipAssertions()) {
            return;
        }

        try {
            for(AssertionEntry assertEntry : assertions) {
                AssertionFunction function;

                if(assertEntry.getType() != null) {
                    function = findByName(assertEntry.getType().name());
                } else {
                    function = registerAssertFunction(assertEntry.getAssertClass());
                }

                String message = replaceAllInContent(settings, assertEntry.getMessage());
                String[] args = Arrays.stream(assertEntry.getArgs())
                    .parallel()
                    .map(arg -> replaceAllInContent(settings, arg))
                    .toArray(String[]::new);

                function.apply(message, args);
            }
        } catch(AssertException ex) {

        }
    }
}
