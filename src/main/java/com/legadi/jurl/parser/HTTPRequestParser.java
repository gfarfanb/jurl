package com.legadi.jurl.parser;

import static com.legadi.jurl.common.CommonUtils.getAllFields;
import static com.legadi.jurl.common.CommonUtils.isBlank;
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

    private static final String DEFAULT_REQUEST_PATTERN = "^(?i)@default-request[ ]*=(.*)";
    private static final String DEFAULT_FLOW_PATTERN = "^(?i)@default-flow[ ]*=(.*)";
    private static final String API_SPEC_PATTERN = "^(?i)###[ ]*api:(.*)";
    private static final String REQUEST_SPEC_PATTERN = "^###(.*)";
    private static final String URL_PATTERN = "^(?i)(http|https):\\/\\/(.*)";
    private static final String HEADER_PATTERN = "^([\\w-]+): (.*)";
    private static final String QUERY_PARAM_PATTERN = "^[&]*([\\w:.-_@]+)=(.*)";
    private static final String FIELD_PATTERN = "^@([\\w]+)[ ]*=(.*)";
    private static final String PARAM_VALUE_PATTERN = "^([\\w:.-_@]+)[ ]*=(.*)";
    private static final String ASSERTION_PATTERN = "^([\\w:.-_@]+)! (.*)";
    private static final String SECTION_PATTERN = "^(?i)\\/\\/[ ]*\\[(main|conditions|file|output|assertions|mock)\\].*";
    private static final String FLOW_SPEC_PATTERN = "^(?i)###[ ]*flow:(.*)";
    private static final String STEP_SPEC_PATTERN = "^(?i)-[ ]*step (.*)";
    private static final String COMMENT_PATTERN = "^#(.*)";

    private static final Map<String, Field> REQUEST_FIELDS = getAllFields(HTTPRequestEntry.class);
    private static final Map<String, Field> REQUEST_FILE_FIELDS = getAllFields(HTTPRequestFileEntry.class);
    private static final Map<String, Field> MOCK_FIELDS = getAllFields(HTTPMockEntry.class);

    private final Pattern defaultRequestPattern = Pattern.compile(DEFAULT_REQUEST_PATTERN);
    private final Pattern defaultFlowPattern = Pattern.compile(DEFAULT_FLOW_PATTERN);
    private final Pattern apiSpecPattern = Pattern.compile(API_SPEC_PATTERN);
    private final Pattern requestSpecPattern = Pattern.compile(REQUEST_SPEC_PATTERN);
    private final Pattern urlPattern = Pattern.compile(URL_PATTERN);
    private final Pattern headerPattern = Pattern.compile(HEADER_PATTERN);
    private final Pattern queryParamPattern = Pattern.compile(QUERY_PARAM_PATTERN);
    private final Pattern fieldPattern = Pattern.compile(FIELD_PATTERN);
    private final Pattern paramValuePattern = Pattern.compile(PARAM_VALUE_PATTERN);
    private final Pattern assertionPattern = Pattern.compile(ASSERTION_PATTERN);
    private final Pattern sectionPattern = Pattern.compile(SECTION_PATTERN);
    private final Pattern flowSpecPattern = Pattern.compile(FLOW_SPEC_PATTERN);
    private final Pattern stepSpecPattern = Pattern.compile(STEP_SPEC_PATTERN);
    private final Pattern commentPattern = Pattern.compile(COMMENT_PATTERN);

    @Override
    public String type() {
        return "http";
    }

    @Override
    public RequestInput<HTTPRequestEntry> parseInput(Settings settings, Path requestPath) {
        try {
            List<String> lines = Files.readAllLines(requestPath);
            String content = String.join(System.lineSeparator(), lines);
            StringExpander stringExpander = new StringExpander(settings);

            content = stringExpander.replaceAllInContent(content);

            String[] parsedLines = content.split(System.lineSeparator());
            Section section = Section.DEFAULT;

            RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
            AtomicReference<HTTPRequestEntry> apiCarrier = new AtomicReference<>(new HTTPRequestEntry());
            AtomicReference<HTTPRequestEntry> requestCarrier = new AtomicReference<>();
            AtomicReference<Pair<String, List<StepEntry>>> flowCarrier = new AtomicReference<>();
            AtomicReference<Section> sectionCarrier = new AtomicReference<>();

            requestInput.setApi(apiCarrier.get());

            for(String line : parsedLines) {
                line = trim(line);

                if(isBlank(line)) {
                    continue;
                }
                if(addDefaultRequest(requestInput, line)) {
                    continue;
                }
                if(addDefaultFlow(requestInput, line)) {
                    continue;
                }

                if(isAPISpec(line)) {
                    section = Section.API;
                    continue;
                }
                if(isRequestSpec(requestCarrier, line)) {
                    section = Section.REQUEST;
                    requestInput.getRequests().put(requestCarrier.get().getName(), requestCarrier.get());
                    continue;
                }
                if(isSection(sectionCarrier, line)) {
                    section = sectionCarrier.get();
                    continue;
                }
                if(isFlowSpec(flowCarrier, line)) {
                    section = Section.FLOW;
                    requestInput.getFlows().put(flowCarrier.get().getLeft(), flowCarrier.get().getRight());
                    continue;
                }

                switch(section) {
                    case API:
                        decorateRequest(apiCarrier, line);
                        break;
                    case REQUEST:
                    case MAIN:
                        decorateRequest(requestCarrier, line);
                        break;
                    case CONDITIONS:
                        break;
                    case FILE:
                        decorateRequestFile(requestCarrier, line);
                        break;
                    case OUTPUT:
                        validateRequest(requestCarrier);
                        if(addParamValue(requestCarrier.get().getOutputMappings(), line)) {
                            continue;
                        }
                        break;
                    case ASSERTIONS:
                        validateRequest(requestCarrier);
                        if(addAssertion(requestCarrier.get(), line)) {
                            continue;
                        }
                        break;
                    case MOCK:
                        decorateMock(requestCarrier, line);
                        break;
                    case FLOW:
                        decorateFlow(flowCarrier, line);
                        break;
                    default:
                        throw new CommandException("Section in request input is not defined yet");
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
            String content = String.join(System.lineSeparator(), lines);
            StringExpander stringExpander = new StringExpander(settings);

            content = stringExpander.replaceAllInContent(content);

            String[] parsedLines = content.split(System.lineSeparator());

            AtomicReference<HTTPRequestEntry> requestCarrier = new AtomicReference<>(new HTTPRequestEntry());

            for(String line : parsedLines) {
                line = trim(line);

                if(isBlank(line)) {
                    continue;
                }

                decorateRequest(requestCarrier, line);
            }

            return requestCarrier.get();
        } catch(IllegalAccessException | IOException ex) {
            throw new IllegalStateException("Unable to process override request file: " + requestPath, ex);
        }
    }

    private void decorateRequest(AtomicReference<HTTPRequestEntry> requestCarrier, String line)
            throws IllegalAccessException {
        validateRequest(requestCarrier);

        if(addURL(requestCarrier.get(), line)) {
            return;
        }
        if(addHeader(requestCarrier.get().getHeaders(), line)) {
            return;
        }
        if(addQueryParam(requestCarrier.get(), line)) {
            return;
        }
        if(addField(REQUEST_FIELDS, requestCarrier.get(), line)) {
            return;
        }
        if(isComment(line)) {
            return;
        }

        HTTPRequestEntry request = requestCarrier.get();
        String body = request.getBodyContent();

        if(isBlank(body)) {
            request.setBodyContent(line);
        } else {
            request.setBodyContent(body + System.lineSeparator() + line);
        }
    }

    private void decorateRequestFile(AtomicReference<HTTPRequestEntry> requestCarrier, String line)
            throws IllegalAccessException {
        validateRequest(requestCarrier);

        if(requestCarrier.get().getRequestFile() == null) {
            requestCarrier.get().setRequestFile(new HTTPRequestFileEntry());
        }

        if(addField(REQUEST_FILE_FIELDS, requestCarrier.get().getRequestFile(), line)) {
            return;
        }
        if(addParamValue(requestCarrier.get().getRequestFile().getFormData(), line)) {
            return;
        }
    }

    private void decorateMock(AtomicReference<HTTPRequestEntry> requestCarrier, String line)
            throws IllegalAccessException {
        validateRequest(requestCarrier);

        if(requestCarrier.get().getMockDefinition() == null) {
            requestCarrier.get().setMockDefinition(new HTTPMockEntry());
        }

        if(addHeader(requestCarrier.get().getMockDefinition().getResponseHeaders(), line)) {
            return;
        }
        if(addField(MOCK_FIELDS, requestCarrier.get(), line)) {
            return;
        }
        if(isComment(line)) {
            return;
        }

        HTTPMockEntry mock = requestCarrier.get().getMockDefinition();
        String response = mock.getResponseContent();

        if(isBlank(response)) {
            mock.setResponseContent(line);
        } else {
            mock.setResponseContent(response + System.lineSeparator() + line);
        }
    }

    private void decorateFlow(AtomicReference<Pair<String, List<StepEntry>>> flowCarrier, String line) {
        if(flowCarrier.get() == null) {
            throw new CommandException("Request file must define a flow section:\n### flow:<flow-name>");
        }

        if(addStep(flowCarrier.get().getRight(), line)) {
            return;
        }
    }

    private boolean addDefaultRequest(RequestInput<HTTPRequestEntry> requestInput, String line) {
        Matcher matcher = defaultRequestPattern.matcher(line);

        if(matcher.find()) {
            requestInput.setDefaultRequest(trim(matcher.group(0)));
            return true;
        } else {
            return false;
        }
    }

    private boolean addDefaultFlow(RequestInput<HTTPRequestEntry> requestInput, String line) {
        Matcher matcher = defaultFlowPattern.matcher(line);

        if(matcher.find()) {
            requestInput.setDefaultFlow(trim(matcher.group(0)));
            return true;
        } else {
            return false;
        }
    }

    private boolean isAPISpec(String line) {
        Matcher matcher = apiSpecPattern.matcher(line);
        return matcher.matches();
    }

    private boolean isRequestSpec(AtomicReference<HTTPRequestEntry> requestCarrier, String line) {
        Matcher matcher = requestSpecPattern.matcher(line);

        if(matcher.find()) {
            HTTPRequestEntry request = new HTTPRequestEntry();
            request.setName(trim(matcher.group(1)));
            requestCarrier.set(request);
            return true;
        } else {
            return false;
        }
    }

    private boolean isSection(AtomicReference<Section> sectionCarrier, String line) {
        Matcher matcher = sectionPattern.matcher(line);

        if(matcher.find()) {
            String name = trim(matcher.group(1));
            sectionCarrier.set(Section.valueOf(name.toUpperCase()));
            return true;
        } else {
            return false;
        }
    }

    private boolean isFlowSpec(AtomicReference<Pair<String, List<StepEntry>>> flowCarrier, String line) {
        Matcher matcher = flowSpecPattern.matcher(line);

        if(matcher.find()) {
            String name = trim(matcher.group(0));
            Pair<String, List<StepEntry>> flow = new Pair<>(name, new LinkedList<>());
            flowCarrier.set(flow);
            return true;
        } else {
            return false;
        }
    }

    private boolean addURL(HTTPRequestEntry request, String line) {
        Matcher matcher = urlPattern.matcher(line);

        if(matcher.matches()) {
            request.setUrl(line);
            return true;
        } else {
            return false;
        }
    }

    private boolean addHeader(Map<String, String> headers, String line) {
        Matcher matcher = headerPattern.matcher(line);

        if(matcher.find()) {
            String header = trim(matcher.group(1));
            String value = trim(matcher.group(2));
            headers.put(header, value);
            return true;
        } else {
            return false;
        }
    }

    private boolean addQueryParam(HTTPRequestEntry request, String line) {
        Matcher matcher = queryParamPattern.matcher(line);

        if(matcher.find()) {
            String queryParam = trim(matcher.group(1));
            String value = trim(matcher.group(2));
            request.getQueryParams().put(queryParam, value);
            return true;
        } else {
            return false;
        }
    }

    private boolean addField(Map<String, Field> fields, Object object, String line)
            throws IllegalAccessException {
        Matcher matcher = fieldPattern.matcher(line);

        if(matcher.find()) {
            String fieldName = trim(matcher.group(1));
            String value = trim(matcher.group(2));
            Field field = fields.get(fieldName);
            if(field == null) {
                throw new CommandException("Field not defined in " +object.getClass() + ": " + fieldName);
            }
            field.setAccessible(true);
            field.set(object, value);
            return true;
        } else {
            return false;
        }
    }

    private boolean addParamValue(Map<String, String> values, String line) {
        Matcher matcher = paramValuePattern.matcher(line);

        if(matcher.find()) {
            String param = trim(matcher.group(1));
            String value = trim(matcher.group(2));
            values.put(param, value);
            return true;
        } else {
            return false;
        }
    }

    private boolean addAssertion(HTTPRequestEntry request, String line) {
        Matcher matcher = assertionPattern.matcher(line);

        if(matcher.find()) {
            String assertion = trim(matcher.group(1));
            String[] args = extractArgs(trim(matcher.group(2)));
            AssertionEntry entry = new AssertionEntry();
            AssertionType type = AssertionType.valueOfName(assertion);

            if(type != null) {
                entry.setType(type);
            } else {
                entry.setAssertionClass(assertion);
            }

            entry.setArgs(args);
            request.getAssertions().add(entry);
            return true;
        } else {
            return false;
        }
    }

    private boolean addStep(List<StepEntry> steps, String line) {
        Matcher matcher = stepSpecPattern.matcher(line);

        if(matcher.find()) {
            String[] args = extractArgs(trim(matcher.group(0)));
            OptionsReader optionsReader = new OptionsReader(args);
            StepEntry step = new StepEntry();
            step.setOptions(optionsReader.getOptionEntries());
            step.setRequestInputPath(optionsReader.getRequestInputPath());
            steps.add(step);
            return true;
        } else {
            return false;
        }
    }

    private boolean isComment(String line) {
        Matcher matcher = commentPattern.matcher(line);
        return matcher.matches();
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

    private void validateRequest(AtomicReference<HTTPRequestEntry> requestCarrier) {
        if(requestCarrier.get() == null) {
            throw new CommandException("Request file must define a request section:### <request-name>");
        }
    }

    public enum Section {

        DEFAULT, API, REQUEST, MAIN, CONDITIONS, FILE, OUTPUT, ASSERTIONS, MOCK, FLOW
    }
}
