package com.legadi.jurl.parser;

import static com.legadi.jurl.common.CommonUtils.getAllFields;
import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.CommonUtils.trim;

import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.common.StringExpander;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.AssertionEntry;
import com.legadi.jurl.model.AssertionType;
import com.legadi.jurl.model.RequestInput;
import com.legadi.jurl.model.StepEntry;
import com.legadi.jurl.model.http.HTTPMockEntry;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.jurl.options.OptionsReader;

public class HTTPRequestParser implements RequestParser<HTTPRequestEntry> {

    private static final Map<String, Field> REQUEST_FIELDS = getAllFields(HTTPRequestEntry.class);
    private static final Map<String, Field> REQUEST_FILE_FIELDS = getAllFields(HTTPRequestFileEntry.class);
    private static final Map<String, Field> MOCK_FIELDS = getAllFields(HTTPMockEntry.class);

    public enum LinePattern {

        EMPTY(""),
        DEFAULT_TYPE("^(?i)@default-(request|flow)[ ]*=(.*)"),
        SECTION("^(?i)###[ ]*\\[(api|request|flow)\\](.*)"),
        REQUEST_FIELD("^(?i)@([\\w:.\\-_@~]+)[ ]*=(.*)"),
        FIELD_VARIABLE("^(?i)(file|mock|output)[ ]*[@]?([\\w:.\\-_@~]+)[ ]*=(.*)"),
        URL_METHOD("^(?i)([\\w]+)?[ ]*(http|https):\\/\\/(.*)"),
        HEADER("^(?i)(mock)?[ ]*([\\w\\-]+): (.*)"),
        QUERY_PARAM("^&([\\w:.\\-_@~]+)=(.*)"),
        CONDITION_ASSERTION("^(?i)(condition|assert) ([\\w:.\\-_@~]+) (.*)"),
        STEP_SPEC("^(?i)step (.*)"),
        COMMENT("^#(.*)");

        private final Pattern pattern;

        private LinePattern(String pattern) {
            this.pattern = Pattern.compile(pattern);
        }

        public Pattern getPattern() {
            return pattern;
        }
    }

    @Override
    public String type() {
        return "http";
    }

    @Override
    public RequestInput<HTTPRequestEntry> parseInput(Settings settings, Path requestPath) {
        try {
            List<String> lines = Files.readAllLines(requestPath);
            StringExpander stringExpander = new StringExpander(settings);
            Section section = Section.DEFAULT;

            RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
            AtomicReference<HTTPRequestEntry> apiCarrier = new AtomicReference<>(new HTTPRequestEntry());
            AtomicReference<HTTPRequestEntry> requestCarrier = new AtomicReference<>();
            AtomicReference<Pair<String, List<StepEntry>>> flowCarrier = new AtomicReference<>();

            requestInput.setApi(apiCarrier.get());

            for(String line : lines) {
                try {
                    readEmptyLine(line);
                    readSection(requestCarrier, flowCarrier, requestInput, line);
                    readComment(line);

                    addDefaultType(stringExpander, requestInput, line);

                    switch(section) {
                        case API:
                            decorateRequest(stringExpander, apiCarrier, line);
                            break;
                        case REQUEST:
                            decorateRequest(stringExpander, requestCarrier, line);
                            break;
                        case FLOW:
                            decorateFlow(stringExpander, flowCarrier, line);
                            break;
                        default:
                            throw new CommandException("Section in request input is not defined yet");
                    }
                } catch(ParsedLineException ex) {
                    section = ex.getSection() != null ? ex.getSection() : section;
                }
            }

            return requestInput;
        } catch(IllegalAccessException | IOException ex) {
            throw new IllegalStateException("Unable to process override request file: " + requestPath, ex);
        }
    }

    @Override
    public HTTPRequestEntry parseRequest(Settings settings, Path requestPath) {
       try {
            List<String> lines = Files.readAllLines(requestPath);
            StringExpander stringExpander = new StringExpander(settings);

            AtomicReference<HTTPRequestEntry> requestCarrier = new AtomicReference<>(new HTTPRequestEntry());

            for(String line : lines) {
                try {
                    readEmptyLine(line);
                    readComment(line);

                    line = stringExpander.replaceAllInContent(line);

                    decorateRequest(stringExpander, requestCarrier, line);
                } catch(ParsedLineException ex) {}
            }

            return requestCarrier.get();
        } catch(IllegalAccessException | IOException ex) {
            throw new IllegalStateException("Unable to process override request file: " + requestPath, ex);
        }
    }

    private void decorateRequest(StringExpander stringExpander,
            AtomicReference<HTTPRequestEntry> requestCarrier, String line)
            throws IllegalAccessException {
        if(requestCarrier.get() == null) {
            throw new CommandException("Request file must define a request section:\n### <request-name>");
        }

        HTTPRequestEntry request = requestCarrier.get();
        Supplier<HTTPRequestFileEntry> fileSupplier = () -> {
            if(request.getRequestFile() == null) {
                request.setRequestFile(new HTTPRequestFileEntry());
            }
            return request.getRequestFile();
        };
        Supplier<HTTPMockEntry> mockSupplier = () -> {
            if(request.getMockDefinition() == null) {
                request.setMockDefinition(new HTTPMockEntry());
            }
            return request.getMockDefinition();
        };

        addURLMethod(stringExpander, request, line);
        addQueryParam(stringExpander, request, line);
        addHeader(stringExpander, request, mockSupplier, line);
        addRequestField(stringExpander, request, line);
        addFieldOrVariable(stringExpander, request, fileSupplier, mockSupplier, line);
        addConditionOrAssertion(stringExpander, request, line);

        String body = request.getBodyContent();
        line = stringExpander.replaceAllInContent(line);

        if(isBlank(body)) {
            request.setBodyContent(line);
        } else {
            request.setBodyContent(body + System.lineSeparator() + line);
        }
    }

    private void decorateFlow(StringExpander stringExpander,
            AtomicReference<Pair<String, List<StepEntry>>> flowCarrier, String line) {
        if(flowCarrier.get() == null) {
            throw new CommandException("Request file must define a flow section:\n### [flow] <flow-name>");
        }

        List<StepEntry> steps = flowCarrier.get().getRight();

        addStep(stringExpander, steps, line);
    }

    private void readEmptyLine(String line) {
        if(isBlank(line)) {
            commit(LinePattern.EMPTY);
        }
    }

    private void addDefaultType(StringExpander stringExpander,
            RequestInput<HTTPRequestEntry> requestInput, String line) {
        LinePattern linePattern = LinePattern.DEFAULT_TYPE;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1));
            String value = stringExpander.replaceAllInContent(
                trim(matcher.group(2))
            );

            if(type.equalsIgnoreCase("request")) {
                requestInput.setDefaultRequest(value);
            } else if(type.equalsIgnoreCase("flow")) {
                requestInput.setDefaultFlow(value);
            }

            commit(linePattern);
        }
    }

    private void readSection(AtomicReference<HTTPRequestEntry> requestCarrier,
            AtomicReference<Pair<String, List<StepEntry>>> flowCarrier,
            RequestInput<HTTPRequestEntry> requestInput, String line) {
        LinePattern linePattern = LinePattern.SECTION;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1)).toLowerCase();
            Section section = null;

            switch(type) {
                case "api":
                    section = Section.API;
                    break;
                case "request":
                    HTTPRequestEntry request = new HTTPRequestEntry();
                    request.setName(trim(matcher.group(2)));
                    requestCarrier.set(request);
                    requestInput.getRequests().put(requestCarrier.get().getName(), requestCarrier.get());
                    section = Section.REQUEST;
                    break;
                case "flow":
                    String name = trim(matcher.group(2));
                    Pair<String, List<StepEntry>> flow = new Pair<>(name, new LinkedList<>());
                    flowCarrier.set(flow);
                    requestInput.getFlows().put(flowCarrier.get().getLeft(), flowCarrier.get().getRight());
                    section = Section.FLOW;
                    break;
            }

            commit(linePattern, section);
        }
    }

    private void addURLMethod(StringExpander stringExpander, HTTPRequestEntry request, String line) {
        LinePattern linePattern = LinePattern.URL_METHOD;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String method = stringExpander.replaceAllInContent(
                trim(matcher.group(1))
            );
            String url = stringExpander.replaceAllInContent(
                trim(matcher.group(2)) + "://" + trim(matcher.group(3))
            );

            if(isNotBlank(method)) {
                request.setMethod(method);
            }

            request.setUrl(url);

            commit(linePattern);
        }
    }

    private void addHeader(StringExpander stringExpander, HTTPRequestEntry request,
            Supplier<HTTPMockEntry> mockSupplier, String line) {
        LinePattern linePattern = LinePattern.HEADER;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1));
            String header = stringExpander.replaceAllInContent(
                trim(matcher.group(2))
            );
            String value = stringExpander.replaceAllInContent(
                trim(matcher.group(3))
            );

            if(type == null) {
                request.getHeaders().put(header, value);
            } else if(type.equalsIgnoreCase("mock")) {
                mockSupplier.get().getResponseHeaders().put(header, value);
            }

            commit(linePattern);
        }
    }

    private void addQueryParam(StringExpander stringExpander, 
            HTTPRequestEntry request, String line) {
        LinePattern linePattern = LinePattern.QUERY_PARAM;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String queryParam = stringExpander.replaceAllInContent(
                trim(matcher.group(1))
            );
            String value = stringExpander.replaceAllInContent(
                trim(matcher.group(2))
            );

            request.getQueryParams().put(queryParam, value);

            commit(linePattern);
        }
    }

    private void addRequestField(StringExpander stringExpander,
            HTTPRequestEntry request, String line) throws IllegalAccessException {
        LinePattern linePattern = LinePattern.REQUEST_FIELD;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String fieldName = stringExpander.replaceAllInContent(
                trim(matcher.group(1))
            );
            String value = stringExpander.replaceAllInContent(
                trim(matcher.group(2))
            );

            addField(REQUEST_FIELDS, request, fieldName, value);

            commit(linePattern);
        }
    }

    private void addFieldOrVariable(StringExpander stringExpander, HTTPRequestEntry request,
            Supplier<HTTPRequestFileEntry> fileSupplier, Supplier<HTTPMockEntry> mockSupplier,
            String line) throws IllegalAccessException {
        LinePattern linePattern = LinePattern.FIELD_VARIABLE;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1));
            String fieldName = stringExpander.replaceAllInContent(
                trim(matcher.group(2))
            );
            String value = trim(matcher.group(3));
            boolean isFieldRequired = line.contains("@");

            if(type.equalsIgnoreCase("file")) {
                value = stringExpander.replaceAllInContent(value);

                if(isFieldRequired) {
                    addField(REQUEST_FILE_FIELDS, fileSupplier.get(), fieldName, value);
                } else {
                    fileSupplier.get().getFormData().put(fieldName, value);
                }
            } else if(type.equalsIgnoreCase("mock")) {
                value = stringExpander.replaceAllInContent(value);

                if(isFieldRequired) {
                    addField(MOCK_FIELDS, mockSupplier.get(), fieldName, value);
                }
            } else if(type.equalsIgnoreCase("output")) {
                request.getOutputMappings().put(fieldName, value);
            }

            commit(linePattern);
        }
    }

    private void addField(Map<String, Field> fields, Object target, String fieldName,
            String value) throws IllegalAccessException {
        Field field = fields.get(fieldName);
        if(field == null) {
            throw new CommandException("Field not defined in " + target.getClass()
                + ": " + fieldName);
        }
        field.setAccessible(true);
        field.set(target, value);
    }

    private void addConditionOrAssertion(StringExpander stringExpander,
            HTTPRequestEntry request, String line) {
        LinePattern linePattern = LinePattern.CONDITION_ASSERTION;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1));
            String assertion = stringExpander.replaceAllInContent(
                trim(matcher.group(2))
            );

            AssertionEntry entry = new AssertionEntry();
            AssertionType assertionType = AssertionType.valueOfName(assertion);

            if(type != null) {
                entry.setType(assertionType);
            } else {
                entry.setAssertionClass(assertion);
            }

            if(type.equalsIgnoreCase("condition")) {
                String[] args = extractArgs(stringExpander.replaceAllInContent(
                    trim(matcher.group(3))
                ));
                entry.setArgs(args);
                request.getConditions().add(entry);
            } else if(type.equalsIgnoreCase("assert")) {
                String[] args = extractArgs(trim(matcher.group(3)));
                entry.setArgs(args);
                request.getAssertions().add(entry);
            }

            commit(linePattern);
        }
    }

    private void addStep(StringExpander stringExpander,
            List<StepEntry> steps, String line) {
        LinePattern linePattern = LinePattern.STEP_SPEC;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String[] args = extractArgs(stringExpander.replaceAllInContent(
                trim(matcher.group(0))
            ));
            OptionsReader optionsReader = new OptionsReader(args);
            StepEntry step = new StepEntry();

            step.setOptions(optionsReader.getOptionEntries());
            step.setRequestInputPath(optionsReader.getRequestInputPath());
            steps.add(step);

            commit(linePattern);
        }
    }

    private void readComment(String line) {
        LinePattern linePattern = LinePattern.COMMENT;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            commit(linePattern);
        }
    }

    private String[] extractArgs(String argsRaw) {
        String[] inputs = argsRaw.split(" ");
        List<String> args = new ArrayList<>();
        StringBuilder argBuilder = new StringBuilder();
        boolean isQuoted = false;

        for(String arg : inputs) {
            if(isQuoted && arg.endsWith("\"")) {
                isQuoted = false;
                
                argBuilder.append(arg);
                args.add(strip(argBuilder.toString(), "\""));

                argBuilder = new StringBuilder();
            } else if(isQuoted || arg.startsWith("\"")) {
                isQuoted = true;

                argBuilder.append(arg).append(' ');
            } else {
                args.add(arg);
            }
        }

        return args.toArray(new String[args.size()]);
    }

    private void commit(LinePattern linePattern) {
        commit(linePattern, null);
    }

    private void commit(LinePattern linePattern, Section section) {
        throw new ParsedLineException(linePattern, section);
    }

    public enum Section {

        DEFAULT, API, REQUEST, FLOW
    }

    public static class ParsedLineException extends RuntimeException {

        private final LinePattern linePattern;
        private final Section section;

        public ParsedLineException(LinePattern linePattern, Section section) {
            this.linePattern = linePattern;
            this.section = section;
        }

        public LinePattern getLinePattern() {
            return linePattern;
        }

        public Section getSection() {
            return section;
        }
    }
}
