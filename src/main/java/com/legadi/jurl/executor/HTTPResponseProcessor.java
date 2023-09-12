package com.legadi.jurl.executor;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.legadi.jurl.assertions.AssertionFunction;
import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.common.StringExpander;
import com.legadi.jurl.exception.AssertionException;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.InvalidAssertionsFoundException;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.executor.reader.JsonOutputReader;
import com.legadi.jurl.executor.reader.XmlOutputReader;
import com.legadi.jurl.model.AssertionEntry;
import com.legadi.jurl.model.HTTPRequestEntry;
import com.legadi.jurl.model.HTTPResponseEntry;
import com.legadi.jurl.model.OutputType;

import static com.legadi.jurl.assertions.AssertionsRegistry.findByName;
import static com.legadi.jurl.assertions.AssertionsRegistry.registerAssertionFunction;
import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.LoaderUtils.loadJsonProperties;
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

        StringExpander stringExpander = new StringExpander(settings);
        Set<String> outputParams = scanOutputParams(stringExpander, request);
        Map<String, String> values = readOutputValues(request.getOutputType(), response, outputParams);

        saveOutput(stringExpander, values, request, response);
        evaluate(stringExpander, values, request.getAssertions());
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

    private Set<String> scanOutputParams(StringExpander stringExpander, HTTPRequestEntry request) {
        Set<String> outputParams = new HashSet<>();

        if(request.getOutputMappings() != null) {
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

        if(request.getAssertions() != null) {
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
        if(request.getOutputMappings() == null) {
            return;
        }

        Map<String, String> outputProperties = request.getOutputMappings().entrySet()
            .parallelStream()
            .map(e -> new Pair<>(
                e.getKey(),
                stringExpander.replaceAllInContent(values, e.getValue())
            ))
            .collect(Collectors.toMap(
                Pair::getLeft,
                Pair::getRight,
                (v1, v2) -> v2,
                HashMap::new
            ));

        Settings.mergeProperties(stringExpander.getSettings().getEnvironment(), outputProperties);

        String overrideFile = stringExpander.getSettings().getOverrideFileName();
        Map<String, String> overrideProperties = loadJsonProperties(overrideFile);

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

    private void evaluate(StringExpander stringExpander, Map<String, String> values,
            List<AssertionEntry> assertions) {
        if(assertions == null || stringExpander.getSettings().isSkipAssertions()) {
            return;
        }

        boolean skip = false;

        for(AssertionEntry assertionEntry : assertions) {
            try {
                AssertionFunction function = null;

                if(assertionEntry.getType() != null) {
                    function = findByName(assertionEntry.getType().name());
                } else if(isBlank(assertionEntry.getAssertionClass())) {
                    throw new CommandException("Assertion class is null or empty, please specify a 'type' or an 'assertionClass'");
                } else {
                    String assertionClass = stringExpander.replaceAllInContent(values,
                        assertionEntry.getAssertionClass());
                    function = registerAssertionFunction(assertionClass);
                }

                String message = stringExpander.replaceAllInContent(values, assertionEntry.getMessage());
                String[] args = Arrays.stream(assertionEntry.getArgs())
                    .parallel()
                    .map(arg -> stringExpander.replaceAllInContent(values, arg))
                    .toArray(String[]::new);

                function.evaluate(message, args);
            } catch(AssertionException ex) {
                LOGGER.info(ex.getMessage());
                skip = true;
            }
        }

        if(skip) {
            throw new InvalidAssertionsFoundException();
        }
    }
}
