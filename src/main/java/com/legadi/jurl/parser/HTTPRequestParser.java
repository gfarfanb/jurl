package com.legadi.jurl.parser;

import static com.legadi.jurl.assertions.AssertionsRegistry.containsName;
import static com.legadi.jurl.common.CommonUtils.getAllFields;
import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.CommonUtils.trim;

import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;
import java.util.logging.Logger;
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
import com.legadi.jurl.model.http.HTTPRequestAuthEntry;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.jurl.options.OptionsReader;

public class HTTPRequestParser implements RequestParser<HTTPRequestEntry> {

    private static final Logger LOGGER = Logger.getLogger(HTTPRequestParser.class.getName());

    private static final Map<String, Field> REQUEST_FIELDS = getAllFields(HTTPRequestEntry.class);
    private static final Map<String, Field> REQUEST_FILE_FIELDS = getAllFields(HTTPRequestFileEntry.class);
    private static final Map<String, Field> REQUEST_AUTH_FIELDS = getAllFields(HTTPRequestAuthEntry.class);
    private static final Map<String, Field> MOCK_FIELDS = getAllFields(HTTPMockEntry.class);

    public enum LinePattern {

        EMPTY(""),
        DEFAULT_TYPE("^(?i)@default-request[ ]*=(.*)"),
        SET_CONFIG("^(?i)@set-([\\w:.\\-_@~]+)[ ]*=(.*)"),
        SECTION("^(?i)###[ ]*\\[(api|request|flow)\\](.*)"),
        SOURCE("^(?i)source (.*)"),
        REQUEST_FIELD("^(?i)@([\\w:.\\-_@~]+)[ ]*=(.*)"),
        FIELD_VARIABLE("^(?i)(file|auth|mock|output)[ ]*[@]?([\\w:.\\-_@~]+)[ ]*=(.*)"),
        URL("^(?i)http.*"),
        URL_METHOD("^(?i)(get|head|post|put|delete|connect|options|trace|patch)[ ]*(.*)"),
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

            RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
            AtomicReference<HTTPRequestEntry> apiCarrier = new AtomicReference<>(new HTTPRequestEntry());
            AtomicReference<HTTPRequestEntry> requestCarrier = new AtomicReference<>();
            AtomicReference<Pair<String, List<StepEntry>>> flowCarrier = new AtomicReference<>();
            Set<String> sectionNames = new HashSet<>();
            Map<String, String> config = new HashMap<>();

            requestInput.setApi(apiCarrier.get());
            requestInput.setConfig(config);

            LOGGER.fine("Processing lines of file: " + requestPath);
            processLines(lines, stringExpander, Section.DEFAULT, requestInput,
                apiCarrier, requestCarrier, flowCarrier, sectionNames, config);

            if(isNotBlank(requestInput.getDefaultRequest())
                    && !sectionNames.contains(requestInput.getDefaultRequest())) {
                throw new CommandException("Default request is not defined: " + requestInput.getDefaultRequest());
            }

            if(isNotEmpty(config)) {
                LOGGER.fine("Loading parsed config [" + settings.getEnvironment() + "]: " + config);
                Settings.mergeProperties(settings.getEnvironment(), config);
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
            Map<String, String> config = new HashMap<>();

            processLines(lines, stringExpander, requestCarrier, config);

            if(isNotEmpty(config)) {
                LOGGER.fine("Loading parsed config [" + settings.getEnvironment() + "]: " + config);
                Settings.mergeProperties(settings.getEnvironment(), config);
            }

            return requestCarrier.get();
        } catch(IllegalAccessException | IOException ex) {
            throw new IllegalStateException("Unable to process override request file: " + requestPath, ex);
        }
    }

    private Section processLines(List<String> lines, StringExpander stringExpander, Section section,
            RequestInput<HTTPRequestEntry> requestInput, AtomicReference<HTTPRequestEntry> apiCarrier,
            AtomicReference<HTTPRequestEntry> requestCarrier,
            AtomicReference<Pair<String, List<StepEntry>>> flowCarrier,
            Set<String> sectionNames, Map<String, String> config)
            throws IllegalAccessException, IOException {

        for(String line : lines) {
            try {
                readEmptyLine(line);
                readSection(sectionNames, requestCarrier, flowCarrier, requestInput, line);
                readSource(line);
                readComment(line);

                addDefaultType(stringExpander, requestInput, config, line);
                addConfig(stringExpander, config, line);

                switch(section) {
                    case API:
                        decorateRequest(stringExpander, apiCarrier, config, line);
                        break;
                    case REQUEST:
                        decorateRequest(stringExpander, requestCarrier, config, line);
                        break;
                    case FLOW:
                        decorateFlow(stringExpander, flowCarrier, config, line);
                        break;
                    default:
                        throw new CommandException("No section (api, request, flow) defined in request input");
                }
            } catch(ParsedLineException ex) {
                section = ex.getSection() != null ? ex.getSection() : section;

                if(ex.getSourcePath() != null) {
                    List<String> sourceLines = Files.readAllLines(ex.getSourcePath());

                    LOGGER.fine("Processing lines of sourced file: " + ex.getSourcePath());
                    section = processLines(sourceLines, stringExpander, section, requestInput,
                        apiCarrier, requestCarrier, flowCarrier, sectionNames, config);
                }
            }
        }

        return section;
    }

    private void processLines(List<String> lines, StringExpander stringExpander,
            AtomicReference<HTTPRequestEntry> requestCarrier, Map<String, String> config)
            throws IllegalAccessException {

        for(String line : lines) {
            try {
                readEmptyLine(line);
                readComment(line);

                addConfig(stringExpander, config, line);

                decorateRequest(stringExpander, requestCarrier, config, line);
            } catch(ParsedLineException ex) {}
        }
    }

    private void decorateRequest(StringExpander stringExpander,
            AtomicReference<HTTPRequestEntry> requestCarrier,
            Map<String, String> config, String line)
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
        Supplier<HTTPRequestAuthEntry> authSupplier = () -> {
            if(request.getRequestAuth() == null) {
                request.setRequestAuth(new HTTPRequestAuthEntry());
            }
            return request.getRequestAuth();
        };
        Supplier<HTTPMockEntry> mockSupplier = () -> {
            if(request.getMockDefinition() == null) {
                request.setMockDefinition(new HTTPMockEntry());
            }
            return request.getMockDefinition();
        };

        addURL(stringExpander, request, config, line);
        addURLMethod(stringExpander, request, config, line);
        addQueryParam(stringExpander, request, config, line);
        addHeader(stringExpander, request, mockSupplier, config, line);
        addRequestField(stringExpander, request, config, line);
        addFieldOrVariable(stringExpander, request,
            fileSupplier, authSupplier, mockSupplier, config, line);
        addConditionOrAssertion(stringExpander, request, config, line);

        String body = request.getBodyContent();
        line = stringExpander.replaceAllInContent(config, line);

        if(isBlank(body)) {
            request.setBodyContent(line);
        } else {
            request.setBodyContent(body + System.lineSeparator() + line);
        }
    }

    private void decorateFlow(StringExpander stringExpander,
            AtomicReference<Pair<String, List<StepEntry>>> flowCarrier,
            Map<String, String> config, String line) {
        if(flowCarrier.get() == null) {
            throw new CommandException("Request file must define a flow section:\n### [flow] <flow-name>");
        }

        List<StepEntry> steps = flowCarrier.get().getRight();

        addStep(stringExpander, steps, config, line);
    }

    private void readEmptyLine(String line) {
        if(isBlank(line)) {
            commit(LinePattern.EMPTY);
        }
    }

    private void addDefaultType(StringExpander stringExpander,
            RequestInput<HTTPRequestEntry> requestInput,
            Map<String, String> config, String line) {
        LinePattern linePattern = LinePattern.DEFAULT_TYPE;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String value = stringExpander.replaceAllInContent(
                config, trim(matcher.group(1))
            );

            requestInput.setDefaultRequest(value);

            commit(linePattern);
        }
    }

    private void addConfig(StringExpander stringExpander,
            Map<String, String> config, String line) {
        LinePattern linePattern = LinePattern.SET_CONFIG;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String property = trim(matcher.group(1));
            String value = stringExpander.replaceAllInContent(
                trim(matcher.group(2))
            );

            config.put(property, value);

            commit(linePattern);
        }
    }

    private void readSection(Set<String> sectionNames,
            AtomicReference<HTTPRequestEntry> requestCarrier,
            AtomicReference<Pair<String, List<StepEntry>>> flowCarrier,
            RequestInput<HTTPRequestEntry> requestInput, String line) {
        LinePattern linePattern = LinePattern.SECTION;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1)).toLowerCase();
            Section section = null;
            String name = null;

            switch(type) {
                case "api":
                    section = Section.API;
                    break;
                case "request":
                    HTTPRequestEntry request = new HTTPRequestEntry();
                    name = trim(matcher.group(2));

                    if(isBlank(name)) {
                        throw new CommandException("Request name is null or empty");
                    }

                    request.setName(name);
                    requestCarrier.set(request);
                    requestInput.getRequests().put(name, requestCarrier.get());
                    section = Section.REQUEST;
                    break;
                case "flow":
                    name = trim(matcher.group(2));

                    if(isBlank(name)) {
                        throw new CommandException("Flow name is null or empty");
                    }

                    Pair<String, List<StepEntry>> flow = new Pair<>(name, new LinkedList<>());
                    flowCarrier.set(flow);
                    requestInput.getFlows().put(name, flowCarrier.get().getRight());
                    section = Section.FLOW;
                    break;
            }

            if(sectionNames.contains(name)) {
                throw new CommandException("Request/Flow name already exists: " + name);
            } else if(isNotBlank(name)) {
                sectionNames.add(name);
            }

            commit(linePattern, section);
        }
    }

    private void readSource(String line) {
        LinePattern linePattern = LinePattern.SOURCE;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String source = trim(matcher.group(1)).toLowerCase();
            Path sourcePath = Paths.get(source);

            commit(linePattern, sourcePath);
        }
    }

    private void addURL(StringExpander stringExpander, HTTPRequestEntry request,
            Map<String, String> config, String line) {
        LinePattern linePattern = LinePattern.URL;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.matches()) {
            String url = stringExpander.replaceAllInContent(
                config, line
            );

            request.setUrl(url);

            commit(linePattern);
        }
    }

    private void addURLMethod(StringExpander stringExpander, HTTPRequestEntry request,
            Map<String, String> config, String line) {
        LinePattern linePattern = LinePattern.URL_METHOD;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String method = stringExpander.replaceAllInContent(
                config, trim(matcher.group(1))
            );
            String url = stringExpander.replaceAllInContent(
                config, trim(matcher.group(2))
            );

            if(isNotBlank(method)) {
                request.setMethod(method);
            }

            request.setUrl(url);

            commit(linePattern);
        }
    }

    private void addHeader(StringExpander stringExpander, HTTPRequestEntry request,
            Supplier<HTTPMockEntry> mockSupplier, Map<String, String> config, String line) {
        LinePattern linePattern = LinePattern.HEADER;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1));
            String header = stringExpander.replaceAllInContent(
                config, trim(matcher.group(2))
            );
            String value = stringExpander.replaceAllInContent(
                config, trim(matcher.group(3))
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
            HTTPRequestEntry request, Map<String, String> config, String line) {
        LinePattern linePattern = LinePattern.QUERY_PARAM;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String queryParam = stringExpander.replaceAllInContent(
                config, trim(matcher.group(1))
            );
            String value = stringExpander.replaceAllInContent(
                config, trim(matcher.group(2))
            );

            request.getQueryParams().put(queryParam, value);

            commit(linePattern);
        }
    }

    private void addRequestField(StringExpander stringExpander,
            HTTPRequestEntry request, Map<String, String> config, String line)
            throws IllegalAccessException {
        LinePattern linePattern = LinePattern.REQUEST_FIELD;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String fieldName = stringExpander.replaceAllInContent(
                config, trim(matcher.group(1))
            );
            String value = stringExpander.replaceAllInContent(
                config, trim(matcher.group(2))
            );

            addField(REQUEST_FIELDS, request, fieldName, value);

            commit(linePattern);
        }
    }

    private void addFieldOrVariable(StringExpander stringExpander,
            HTTPRequestEntry request,
            Supplier<HTTPRequestFileEntry> fileSupplier,
            Supplier<HTTPRequestAuthEntry> authSupplier,
            Supplier<HTTPMockEntry> mockSupplier,
            Map<String, String> config, String line) throws IllegalAccessException {
        LinePattern linePattern = LinePattern.FIELD_VARIABLE;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1));
            String fieldName = stringExpander.replaceAllInContent(
                config, trim(matcher.group(2))
            );
            String value = trim(matcher.group(3));
            boolean isFieldRequired = line.contains("@");

            if(type.equalsIgnoreCase("file")) {
                value = stringExpander.replaceAllInContent(config, value);

                if(isFieldRequired) {
                    addField(REQUEST_FILE_FIELDS, fileSupplier.get(), fieldName, value);
                } else {
                    fileSupplier.get().getFormData().put(fieldName, value);
                }
            } else if(type.equalsIgnoreCase("auth")) {
                value = stringExpander.replaceAllInContent(config, value);

                if(isFieldRequired) {
                    addField(REQUEST_AUTH_FIELDS, authSupplier.get(), fieldName, value);
                }
            } else if(type.equalsIgnoreCase("mock")) {
                value = stringExpander.replaceAllInContent(config, value);

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
            HTTPRequestEntry request, Map<String, String> config, String line) {
        LinePattern linePattern = LinePattern.CONDITION_ASSERTION;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1));
            String assertion = stringExpander.replaceAllInContent(
                config, trim(matcher.group(2))
            );

            AssertionEntry entry = new AssertionEntry();

            if(containsName(assertion)) {
                entry.setName(assertion);
            } else {
                entry.setAssertionClass(assertion);
            }

            if(type.equalsIgnoreCase("condition")) {
                String[] args = extractArgs(stringExpander.replaceAllInContent(
                    config, trim(matcher.group(3))
                ));
                entry.setArgs(args);
                entry.setType(AssertionType.CONDITION);
                request.getConditions().add(entry);
            } else if(type.equalsIgnoreCase("assert")) {
                String[] args = extractArgs(trim(matcher.group(3)));
                entry.setArgs(args);
                entry.setType(AssertionType.ASSERTION);
                request.getAssertions().add(entry);
            }

            commit(linePattern);
        }
    }

    private void addStep(StringExpander stringExpander,
            List<StepEntry> steps, Map<String, String> config, String line) {
        LinePattern linePattern = LinePattern.STEP_SPEC;
        Matcher matcher = linePattern.getPattern().matcher(line);

        if(matcher.find()) {
            String[] args = extractArgs(stringExpander.replaceAllInContent(
                config, trim(matcher.group(1))
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
        throw new ParsedLineException(linePattern, null, null);
    }

    private void commit(LinePattern linePattern, Path sourcePath) {
        throw new ParsedLineException(linePattern, null, sourcePath);
    }

    private void commit(LinePattern linePattern, Section section) {
        throw new ParsedLineException(linePattern, section, null);
    }

    public enum Section {

        DEFAULT, API, REQUEST, FLOW
    }

    public static class ParsedLineException extends RuntimeException {

        private final LinePattern linePattern;
        private final Section section;
        private final Path sourcePath;

        public ParsedLineException(LinePattern linePattern, Section section, Path sourcePath) {
            this.linePattern = linePattern;
            this.section = section;
            this.sourcePath = sourcePath;
        }

        public LinePattern getLinePattern() {
            return linePattern;
        }

        public Section getSection() {
            return section;
        }

        public Path getSourcePath() {
            return sourcePath;
        }
    }
}
