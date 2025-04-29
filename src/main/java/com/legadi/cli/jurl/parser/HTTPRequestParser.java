package com.legadi.cli.jurl.parser;

import static com.legadi.cli.jurl.common.CommonUtils.getAllFields;
import static com.legadi.cli.jurl.common.CommonUtils.getDefaultFieldIndex;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.cli.jurl.common.CommonUtils.strip;
import static com.legadi.cli.jurl.common.CommonUtils.trim;
import static com.legadi.cli.jurl.common.ObjectsRegistry.containsName;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findAll;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;

import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.legadi.cli.jurl.assertions.AssertionFunction;
import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.exception.RequestException;
import com.legadi.cli.jurl.executor.HeaderAuthenticator;
import com.legadi.cli.jurl.model.AssertionEntry;
import com.legadi.cli.jurl.model.AssertionType;
import com.legadi.cli.jurl.model.FlowEntry;
import com.legadi.cli.jurl.model.RequestInput;
import com.legadi.cli.jurl.model.StepEntry;
import com.legadi.cli.jurl.model.http.HTTPMockEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.cli.jurl.options.Option;
import com.legadi.cli.jurl.options.OptionsReader;
import com.legadi.cli.jurl.options.OptionsReader.OptionEntry;

public class HTTPRequestParser implements RequestParser<HTTPRequestEntry> {

    private static final Logger LOGGER = Logger.getLogger(HTTPRequestParser.class.getName());

    private static final Map<String, Field> REQUEST_FIELDS = getAllFields(HTTPRequestEntry.class);
    private static final Map<String, Field> REQUEST_FILE_FIELDS = getAllFields(HTTPRequestFileEntry.class);
    private static final String REQUEST_FILE_FIELD_PATH = "path";
    private static final String REQUEST_FILE_FIELD_FIELD = "field";
    private static final Map<String, Field> MOCK_FIELDS = getAllFields(HTTPMockEntry.class);

    private static final Pattern NAME_DESCRIPTION_PATTERN = Pattern.compile("^(?i)(.*)[ ]*:(.*)");

    private static final String ALL_ENVS = "all";

    public enum LinePattern {

        EMPTY(""),
        DEFAULT_TYPE("^(?i)@default-request[ ]*=(.*)"),
        SECTION("^(?i)###[ ]*\\[(api|request|flow)\\](.*)"),
        SOURCE("^(?i)source (.*)"),
        REQUEST_FIELD("^(?i)@([\\w:.\\-_@~]+)[ ]*=(.*)"),
        SET_DEFAULT("^(?i)@(set|list|list\\*)-([\\w:.\\-_@~]+)[ ]*=(.*)"),
        FIELD_VARIABLE("^(?i)(file|%s|mock|form|output)[ ]*[@]?([\\w:.\\-_@~]+)[ ]*=(.*)"),
        URL("^(?i)http.*"),
        URL_METHOD("^(?i)(get|head|post|put|delete|connect|options|trace|patch)[ ]+(.*)"),
        HEADER("^(?i)(mock)?[ ]*([\\w\\-]+): (.*)"),
        QUERY_PARAM("^&([\\w:.\\-_@~]+)=(.*)"),
        CONDITION_ASSERTION_OPT("^(?i)(condition|assert|opt) ([\\w:.\\-_@~]+)(.*)"),
        STEP_SPEC("^(?i)step (.*)"),
        COMMENT("^#(.*)"),
        ENV_LABEL("^:(.*)");

        private final String regex;
        private final Pattern pattern;

        private LinePattern(String regex) {
            this.regex = regex;
            this.pattern = Pattern.compile(regex);
        }

        public String getRegex() {
            return regex;
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

            RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
            HTTPRequestEntry apiRequest = new HTTPRequestEntry();
            AtomicReference<Pair<HTTPRequestEntry, RequestFileSupplier>> apiCarrier =
                new AtomicReference<>(new Pair<>(apiRequest, new RequestFileSupplier(apiRequest)));
            AtomicReference<Pair<HTTPRequestEntry, RequestFileSupplier>> requestCarrier = new AtomicReference<>();
            AtomicReference<FlowEntry> flowCarrier = new AtomicReference<>();
            Set<String> labeledEnvs = new HashSet<>();
            Set<String> sectionNames = new HashSet<>();

            requestInput.setApi(apiCarrier.get().getLeft());

            LOGGER.fine("Processing lines of file: " + requestPath);
            processLines(settings, lines, Section.DEFAULT, requestInput,
                apiCarrier, requestCarrier, flowCarrier, labeledEnvs, sectionNames);

            if(isNotBlank(requestInput.getDefaultRequest())
                    && !sectionNames.contains(requestInput.getDefaultRequest())) {
                throw new CommandException("Request is not defined for: " + requestInput.getDefaultRequest());
            }

            return requestInput;
        } catch(CommandException ex) {
            throw ex;
        } catch(IllegalArgumentException | IOException ex) {
            throw new IllegalStateException("Unable to parse input request file: " + requestPath, ex);
        }
    }

    @Override
    public HTTPRequestEntry parseRequest(Settings settings, Path requestPath) {
       try {
            List<String> lines = Files.readAllLines(requestPath);

            HTTPRequestEntry request =  new HTTPRequestEntry();
            AtomicReference<Pair<HTTPRequestEntry, RequestFileSupplier>> requestCarrier =
                new AtomicReference<>(new Pair<>(request, new RequestFileSupplier(request)));
            Set<String> labeledEnvs = new HashSet<>();

            processLines(settings, lines, requestCarrier, labeledEnvs);

            return requestCarrier.get().getLeft();
        } catch(CommandException ex) {
            throw ex;
        } catch(IllegalArgumentException | IOException ex) {
            throw new IllegalStateException("Unable to process override request file: " + requestPath, ex);
        }
    }

    private Section processLines(Settings settings, List<String> lines, Section section,
            RequestInput<HTTPRequestEntry> requestInput,
            AtomicReference<Pair<HTTPRequestEntry, RequestFileSupplier>> apiCarrier,
            AtomicReference<Pair<HTTPRequestEntry, RequestFileSupplier>> requestCarrier,
            AtomicReference<FlowEntry> flowCarrier,
            Set<String> labeledEnvs, Set<String> sectionNames) throws IOException {

        for(String line : lines) {
            try {
                readEmptyLine(line);
                readEnvLabel(settings, labeledEnvs, line);
                readSection(labeledEnvs, sectionNames, requestCarrier, flowCarrier, requestInput, line);
                readSource(line);
                readComment(line);

                addDefaultType(requestInput, line);

                switch(section) {
                    case API:
                        decorateRequest(settings, apiCarrier, line);
                        break;
                    case REQUEST:
                        decorateRequest(settings, requestCarrier, line);
                        break;
                    case FLOW:
                        decorateFlow(flowCarrier, line);
                        break;
                    default:
                        throw new CommandException("No section (api, request, flow) defined in request input");
                }
            } catch(ParsedLineException ex) {
                section = ex.getSection() != null ? ex.getSection() : section;

                if(ex.getSourcePath() != null) {
                    List<String> sourceLines = Files.readAllLines(ex.getSourcePath());

                    LOGGER.fine("[" + ex.getLinePattern() + "] Processing lines of sourced file: " + ex.getSourcePath());
                    section = processLines(settings, sourceLines, section, requestInput,
                        apiCarrier, requestCarrier, flowCarrier, labeledEnvs, sectionNames);
                }
            }
        }

        return section;
    }

    private void processLines(Settings settings, List<String> lines,
            AtomicReference<Pair<HTTPRequestEntry, RequestFileSupplier>> requestCarrier,
            Set<String> labeledEnvs) {

        for(String line : lines) {
            try {
                readEmptyLine(line);
                readEnvLabel(settings, labeledEnvs, line);
                readComment(line);

                decorateRequest(settings, requestCarrier, line);
            } catch(ParsedLineException ex) {}
        }
    }

    private void decorateRequest(Settings settings,
            AtomicReference<Pair<HTTPRequestEntry, RequestFileSupplier>> requestCarrier,
            String line) {
        HTTPRequestEntry request = requestCarrier.get().getLeft();
        RequestFileSupplier fileSupplier = requestCarrier.get().getRight();
        Supplier<HTTPMockEntry> mockSupplier = () -> {
            if(request.getMockDefinition() == null) {
                request.setMockDefinition(new HTTPMockEntry());
            }
            return request.getMockDefinition();
        };

        addURL(request, line);
        addURLMethod(request, line);
        addQueryParam(request, line);
        addHeader(request, mockSupplier, line);
        addDefault(request.getDefaults(), line);
        addRequestField(request, line);
        addFieldOrVariable(settings, request, fileSupplier, mockSupplier, line);
        addConditionOrAssertion(request, line);

        String body = request.getBodyContent();

        if(isBlank(body)) {
            request.setBodyContent(line);
        } else {
            request.setBodyContent(body + System.lineSeparator() + line);
        }
    }

    private void decorateFlow(AtomicReference<FlowEntry> flowCarrier,
            String line) {
        FlowEntry flow = flowCarrier.get();

        addDefault(flow.getDefaults(), line);
        addStep(flow.getSteps(), line);
    }

    private void readEmptyLine(String line) {
        if(isBlank(line)) {
            throw new ParsedLineException(LinePattern.EMPTY, null, null);
        }
    }

    private void readEnvLabel(Settings settings, Set<String> labeledEnvs, String line) {
        applyLine(LinePattern.ENV_LABEL, line,
            matcher -> {
                String[] envs = trim(matcher.group(1)).toLowerCase().split(",");

                labeledEnvs.clear();
                
                for(String env : envs) {
                    if(isNotBlank(env)) {
                        labeledEnvs.add(env);
                    }
                }

                return null;
            });

        if(labeledEnvs.isEmpty() || labeledEnvs.contains(ALL_ENVS)) {
            return;
        }

        String currentEnv = settings.getEnvironment().toLowerCase();
        boolean envMatches = labeledEnvs
            .stream()
            .map(Pattern::compile)
            .map(p -> p.matcher(currentEnv))
            .anyMatch(Matcher::find);

        if(envMatches) {
            return;
        }

        throw new ParsedLineException(LinePattern.EMPTY, null, null);
    }

    private void addDefaultType(RequestInput<HTTPRequestEntry> requestInput, String line) {
        applyLine(LinePattern.DEFAULT_TYPE, line,
            matcher -> {
                String value = trim(matcher.group(1));

                requestInput.setDefaultRequest(value);

                return null;
            });
    }

    private void readSection(Set<String> labeledEnvs, Set<String> sectionNames,
            AtomicReference<Pair<HTTPRequestEntry, RequestFileSupplier>> requestCarrier,
            AtomicReference<FlowEntry> flowCarrier,
            RequestInput<HTTPRequestEntry> requestInput, String line) {
        applyLine(LinePattern.SECTION, line,
            matcher -> {
                String type = trim(matcher.group(1)).toLowerCase();
                Pair<String, String> nameDescription;
                Section section = null;
                String name = null;

                labeledEnvs.clear();

                switch(type) {
                    case "request":
                        HTTPRequestEntry request = new HTTPRequestEntry();
                        nameDescription = getNameAndDescription(
                            trim(matcher.group(2)));
                        name = nameDescription.getLeft();

                        if(isBlank(name)) {
                            throw new CommandException("Request name is null or empty");
                        }

                        request.setName(name);
                        request.setDescription(nameDescription.getRight());
                        requestCarrier.set(new Pair<>(request, new RequestFileSupplier(request)));
                        requestInput.getRequests().put(name, request);
                        section = Section.REQUEST;
                        break;
                    case "flow":
                        nameDescription = getNameAndDescription(
                            trim(matcher.group(2)));
                        name = nameDescription.getLeft();

                        if(isBlank(name)) {
                            throw new CommandException("Flow name is null or empty");
                        }

                        FlowEntry flow = new FlowEntry();
                        flow.setName(name);
                        flow.setDescription(nameDescription.getRight());
                        flowCarrier.set(flow);
                        requestInput.getFlows().put(name, flow);
                        section = Section.FLOW;
                        break;
                    default:
                        // type = "api"
                        section = Section.API;
                        break;
                }

                if(sectionNames.contains(name)) {
                    throw new CommandException("Request/Flow name already exists: " + name);
                } else if(isNotBlank(name)) {
                    sectionNames.add(name);
                }

                return section;
            });
    }

    private Pair<String, String> getNameAndDescription(String line) {
        Matcher matcher = NAME_DESCRIPTION_PATTERN.matcher(line);

        if(matcher.find()) {
            String name = trim(matcher.group(1));
            String description = trim(matcher.group(2));
            return new Pair<>(name, description);
        } else {
            return new Pair<>(line, null);
        }
    }

    private void readSource(String line) {
        applyLine(LinePattern.SOURCE, line,
            matcher -> {
                String source = trim(matcher.group(1)).toLowerCase();

                return Paths.get(source);
            });
    }

    private void addURL(HTTPRequestEntry request, String line) {
        applyLine(LinePattern.URL, line,
            matcher -> {
                request.setUrl(line);

                return null;
            });
    }

    private void addURLMethod(HTTPRequestEntry request, String line) {
        applyLine(LinePattern.URL_METHOD, line,
            matcher -> {
                String method = trim(matcher.group(1));
                String url = trim(matcher.group(2));

                request.setMethod(method);
                request.setUrl(url);

                return null;
            });
    }

    private void addHeader(HTTPRequestEntry request,
            Supplier<HTTPMockEntry> mockSupplier, String line) {
        applyLine(LinePattern.HEADER, line,
            matcher -> {
                String type = trim(matcher.group(1));
                String header = trim(matcher.group(2));
                String value = trim(matcher.group(3));

                if("mock".equalsIgnoreCase(type)) {
                    mockSupplier.get().getResponseHeaders().put(header, value);
                } else {
                    // type = null
                    request.getHeaders().put(header, value);
                }

                return null;
            });
    }

    private void addQueryParam(HTTPRequestEntry request, String line) {
        applyLine(LinePattern.QUERY_PARAM, line,
            matcher -> {
                String queryParam = trim(matcher.group(1));
                String value = trim(matcher.group(2));

                request.getQueryParams().put(queryParam, value);

                return null;
            });
    }

    private void addRequestField(HTTPRequestEntry request, String line) {
        applyLine(LinePattern.REQUEST_FIELD, line,
            matcher -> {
                String fieldName = trim(matcher.group(1));
                String value = trim(matcher.group(2));

                addField(REQUEST_FIELDS, request, fieldName, value);

                return null;
            });
    }

    private void addFieldOrVariable(Settings settings, HTTPRequestEntry request,
            RequestFileSupplier fileSupplier,
            Supplier<HTTPMockEntry> mockSupplier,
            String line) {
        List<HeaderAuthenticator<?, ?>> headerAuthenticators = findAll(HeaderAuthenticator.class, type());
        Set<String> authElements = headerAuthenticators.stream()
            .map(HeaderAuthenticator::getParserElement)
            .map(String::toLowerCase)
            .collect(Collectors.toSet());
        String regexElementsPart = authElements.stream()
            .collect(Collectors.joining("|"));

        applyLine(LinePattern.FIELD_VARIABLE, line,
            matcher -> {
                String type = trim(matcher.group(1));
                String fieldName = trim(matcher.group(2));
                String value = trim(matcher.group(3));
                boolean isFieldRequired = line.contains("@");

                if("file".equalsIgnoreCase(type)) {
                    if(isFieldRequired) {
                        addFileField(fileSupplier, fieldName, value);
                    }
                } else if(authElements.contains(type.toLowerCase())) {
                    if(isFieldRequired) {
                        HeaderAuthenticator<?, ?> auth = headerAuthenticators.stream()
                            .filter(a -> a.getParserElement().equalsIgnoreCase(type))
                            .findFirst()
                            .get();

                        addField(auth.getObjectFields(),
                            auth.getAuthEntrySupplier(settings, request).get(),
                            fieldName, value);
                    }
                } else if("mock".equalsIgnoreCase(type)) {
                    if(isFieldRequired) {
                        addField(MOCK_FIELDS, mockSupplier.get(), fieldName, value);
                    }
                }  else if("form".equalsIgnoreCase(type)) {
                    request.getFormData().put(fieldName, value);
                } else {
                    // type = "output"
                    request.getOutputMappings().put(fieldName, value);
                }

                return null;
            }, regexElementsPart);
    }

    private void addDefault(Map<String, Object> defaults, String line) {
        applyLine(LinePattern.SET_DEFAULT, line,
            matcher -> {
                String type = trim(matcher.group(1));
                String property = trim(matcher.group(2));
                String value = trim(matcher.group(3));

                if("set".equalsIgnoreCase(type)) {
                    defaults.put(property, value);
                } else if("list*".equalsIgnoreCase(type)) {
                    int index = addValueToDefaultList(defaults, property, value);
                    defaults.put(getDefaultFieldIndex(property), Integer.toString(index));
                } else {
                    // type = "list"
                    addValueToDefaultList(defaults, property, value);
                }

                return null;
            });
    }

    @SuppressWarnings("unchecked")
    private int addValueToDefaultList(Map<String, Object> defaults, String property, String value) {
        List<String> values = (List<String>) defaults.getOrDefault(property, new ArrayList<>());
        values.add(value);
        defaults.putIfAbsent(property, values);
        return values.size() - 1;
    }

    private void addFileField(RequestFileSupplier fileSupplier, String fieldName, String value)
            throws IllegalAccessException {
        HTTPRequestFileEntry requestFile = fileSupplier.get(fieldName, value);
        addField(REQUEST_FILE_FIELDS, requestFile, fieldName, value);
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

    private void addConditionOrAssertion(HTTPRequestEntry request, String line) {
        applyLine(LinePattern.CONDITION_ASSERTION_OPT, line,
            matcher -> {
                String type = trim(matcher.group(1));
                String nameOrClass = trim(matcher.group(2));

                if("condition".equalsIgnoreCase(type)) {
                    String[] args = extractArgs(trim(matcher.group(3)));
                    AssertionEntry entry = createAssertion(nameOrClass);
                    entry.setArgs(args);
                    entry.setType(AssertionType.CONDITION);
                    request.getConditions().add(entry);
                } else if("assert".equalsIgnoreCase(type)) {
                    String[] args = extractArgs(trim(matcher.group(3)));
                    AssertionEntry entry = createAssertion(nameOrClass);
                    entry.setArgs(args);
                    entry.setType(AssertionType.ASSERTION);
                    request.getAssertions().add(entry);
                } else {
                    // type = "opt"
                    Option option = findByNameOrFail(Option.class, nameOrClass);
                    String[] args = extractArgs(trim(matcher.group(3)));
                    request.getOptions().add(new OptionEntry(option, args));
                }

                return null;
            });
    }

    private void addStep(List<StepEntry> steps, String line) {
        applyLine(LinePattern.STEP_SPEC, line,
            matcher -> {
                String[] args = extractArgs(trim(matcher.group(1)));
                OptionsReader optionsReader = new OptionsReader(args);
                StepEntry step = new StepEntry();

                step.setOptions(optionsReader.getOptionEntries());
                step.setRequestInputPath(optionsReader.getRequestInputPath());
                steps.add(step);

                return null;
            });
    }

    private void readComment(String line) {
        applyLine(LinePattern.COMMENT, line,
            matcher -> {
                return null;
            });
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

    private AssertionEntry createAssertion(String nameOrClass) {
        AssertionEntry entry = new AssertionEntry();

        if(containsName(AssertionFunction.class, nameOrClass)) {
            entry.setName(nameOrClass);
        } else {
            entry.setAssertionClass(nameOrClass);
        }

        return entry;
    }

    private static void applyLine(LinePattern linePattern, String line,
            MatcherTransformer<Matcher> matcherConsumer, Object... regexParts) {
        Matcher matcher;

        if(isNotEmpty(regexParts)) {
            String regex = String.format(linePattern.getRegex(), regexParts);
            matcher = Pattern.compile(regex).matcher(line);
        } else {
            matcher = linePattern.getPattern().matcher(line);
        }

        if(matcher.find()) {
            Section section = null;
            Path sourcePath = null;

            try {
                Object result = matcherConsumer.apply(matcher);

                if(result instanceof Section) {
                    section = (Section) result;
                } else if(result instanceof Path) {
                    sourcePath = (Path) result;
                }
            } catch(CommandException ex) {
                throw ex;
            } catch(Exception ex) {
                throw new IllegalArgumentException("Error processing line: '" + line + "'", ex);
            }

            throw new ParsedLineException(linePattern, section, sourcePath);
        }
    }

    public enum Section {

        DEFAULT, API, REQUEST, FLOW
    }

    @FunctionalInterface
    public static interface MatcherTransformer<T> {

        Object apply(T value) throws Exception;
    }

    public static class RequestFileSupplier {

        private Map<String, HTTPRequestFileEntry> requestFilesByPath = new HashMap<>();
        private final HTTPRequestEntry request;

        private HTTPRequestFileEntry lastRequestFile;

        public RequestFileSupplier(HTTPRequestEntry request) {
            this.request = request;
        }

        public HTTPRequestFileEntry get(String fieldName, String value) {
            int index;

            switch(fieldName) {
                case REQUEST_FILE_FIELD_PATH:
                    index = request.getRequestFiles().size();
                    if(isBlank(value)) {
                        throw new RequestException(request,
                            "request.requestFiles[" + index + "].path (file path) is null or empty");
                    }
                    if(requestFilesByPath.get(value) == null) {
                        lastRequestFile = new HTTPRequestFileEntry();
                        requestFilesByPath.put(value, lastRequestFile);
                        request.getRequestFiles().add(lastRequestFile);
                    } else {
                        lastRequestFile = requestFilesByPath.get(value);
                    }
                    break;
                case REQUEST_FILE_FIELD_FIELD:
                    index = request.getRequestFiles().size() - 1;
                    if(isBlank(value)) {
                        throw new RequestException(request,
                            "request.requestFiles[" + index + "].field (form file field) is null or empty");
                    }
                    break;
            }

            if(lastRequestFile == null) {
                throw new RequestException(request,
                    "request.requestFiles[0] is null, define a 'path' first");
            }

            return lastRequestFile;
        }


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
