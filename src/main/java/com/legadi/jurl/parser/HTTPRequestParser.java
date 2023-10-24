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

    private final Pattern defaultTypePattern = Pattern.compile("^(?i)@default-(request|flow)[ ]*=(.*)");
    private final Pattern sectionPattern = Pattern.compile("^(?i)###[ ]*\\[(api|request|flow)\\](.*)");
    private final Pattern fieldPattern = Pattern.compile("^(?i)(file|mock|output)?[ ]*[@]?([\\w:.-_@]+)[ ]*=(.*)");
    private final Pattern urlMethodPattern = Pattern.compile("^(?i)([\\w]+)?[ ]*(http|https):\\/\\/(.*)");
    private final Pattern headerPattern = Pattern.compile("^(?i)(mock)?[ ]*([\\w-]+): (.*)");
    private final Pattern queryParamPattern = Pattern.compile("^[&]*([\\w:.-_@]+)=(.*)");
    private final Pattern assertionPattern = Pattern.compile("^(?i)(condition|assert) ([\\w:.-_@]+) (.*)");
    private final Pattern stepSpecPattern = Pattern.compile("^(?i)step (.*)");
    private final Pattern commentPattern = Pattern.compile("^#(.*)");

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

                if(addDefaultType(requestInput, line)) {
                    continue;
                }
                if(isSection(sectionCarrier, requestCarrier, flowCarrier, requestInput, line)) {
                    section = sectionCarrier.get();
                    continue;
                }

                switch(section) {
                    case API:
                        decorateRequest(apiCarrier, line);
                        break;
                    case REQUEST:
                        decorateRequest(requestCarrier, line);
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

        if(addURLMethod(request, line)) {
            return;
        }
        if(addHeader(request, mockSupplier, line)) {
            return;
        }
        if(addQueryParam(request, line)) {
            return;
        }
        if(addFieldOrVariable(request, fileSupplier, mockSupplier, line)) {
            return;
        }
        if(addConditionOrAssertion(request, line)) {
            return;
        }
        if(isComment(line)) {
            return;
        }

        String body = request.getBodyContent();

        if(isBlank(body)) {
            request.setBodyContent(line);
        } else {
            request.setBodyContent(body + System.lineSeparator() + line);
        }
    }

    private void decorateFlow(AtomicReference<Pair<String, List<StepEntry>>> flowCarrier, String line) {
        if(flowCarrier.get() == null) {
            throw new CommandException("Request file must define a flow section:\n### [flow] <flow-name>");
        }

        List<StepEntry> steps = flowCarrier.get().getRight();

        if(addStep(steps, line)) {
            return;
        }
    }

    private boolean addDefaultType(RequestInput<HTTPRequestEntry> requestInput, String line) {
        Matcher matcher = defaultTypePattern.matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1));
            String value = trim(matcher.group(2));

            if(type.equalsIgnoreCase("request")) {
                requestInput.setDefaultRequest(value);
            } else if(type.equalsIgnoreCase("flow")) {
                requestInput.setDefaultFlow(value);
            }
            return true;
        } else {
            return false;
        }
    }

    private boolean isSection(AtomicReference<Section> sectionCarrier,
            AtomicReference<HTTPRequestEntry> requestCarrier,
            AtomicReference<Pair<String, List<StepEntry>>> flowCarrier,
            RequestInput<HTTPRequestEntry> requestInput, String line) {
        Matcher matcher = sectionPattern.matcher(line);

        if(matcher.find()) {
            String section = trim(matcher.group(1)).toLowerCase();

            switch(section) {
                case "api":
                    sectionCarrier.set(Section.API);
                    break;
                case "request":
                    HTTPRequestEntry request = new HTTPRequestEntry();
                    request.setName(trim(matcher.group(2)));
                    requestCarrier.set(request);
                    requestInput.getRequests().put(requestCarrier.get().getName(), requestCarrier.get());
                    sectionCarrier.set(Section.REQUEST);
                    break;
                case "flow":
                    String name = trim(matcher.group(2));
                    Pair<String, List<StepEntry>> flow = new Pair<>(name, new LinkedList<>());
                    flowCarrier.set(flow);
                    requestInput.getFlows().put(flowCarrier.get().getLeft(), flowCarrier.get().getRight());
                    sectionCarrier.set(Section.FLOW);
                    break;
            }
            return true;
        } else {
            return false;
        }
    }

    private boolean addURLMethod(HTTPRequestEntry request, String line) {
        Matcher matcher = urlMethodPattern.matcher(line);

        if(matcher.find()) {
            String method = trim(matcher.group(1));
            if(isNotBlank(method)) {
                request.setMethod(method);
            }
            request.setUrl(trim(matcher.group(2)) + "://" + trim(matcher.group(3)));
            return true;
        } else {
            return false;
        }
    }

    private boolean addHeader(HTTPRequestEntry request,
            Supplier<HTTPMockEntry> mockSupplier, String line) {
        Matcher matcher = headerPattern.matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1));
            String header = trim(matcher.group(2));
            String value = trim(matcher.group(3));

            if(type == null) {
                request.getHeaders().put(header, value);
            } else if(type.equalsIgnoreCase("mock")) {
                mockSupplier.get().getResponseHeaders().put(header, value);
            }
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

    private boolean addFieldOrVariable(HTTPRequestEntry request,
            Supplier<HTTPRequestFileEntry> fileSupplier, Supplier<HTTPMockEntry> mockSupplier,
            String line) throws IllegalAccessException {
        Matcher matcher = fieldPattern.matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1));
            String fieldName = trim(matcher.group(2));
            String value = trim(matcher.group(3));
            boolean isFieldRequired = line.contains("@");

            if(type == null && isFieldRequired) {
                addField(REQUEST_FIELDS, request, fieldName, value);
            } else if(type.equalsIgnoreCase("file")) {
                if(isFieldRequired) {
                    addField(REQUEST_FILE_FIELDS, fileSupplier.get(), fieldName, value);
                } else {
                    fileSupplier.get().getFormData().put(fieldName, value);
                }
            } else if(type.equalsIgnoreCase("mock")) {
                if(isFieldRequired) {
                    addField(MOCK_FIELDS, mockSupplier.get(), fieldName, value);
                }
            } else if(type.equalsIgnoreCase("output")) {
                request.getOutputMappings().put(fieldName, value);
            }
            return true;
        } else {
            return false;
        }
    }

    private void addField(Map<String, Field> fields, Object target, String fieldName, String value)
            throws IllegalAccessException {
        Field field = fields.get(fieldName);
        if(field == null) {
            throw new CommandException("Field not defined in " + target.getClass()
                + ": " + fieldName);
        }
        field.setAccessible(true);
        field.set(target, value);
    }

    private boolean addConditionOrAssertion(HTTPRequestEntry request, String line) {
        Matcher matcher = assertionPattern.matcher(line);

        if(matcher.find()) {
            String type = trim(matcher.group(1));
            String assertion = trim(matcher.group(2));
            String[] args = extractArgs(trim(matcher.group(3)));

            AssertionEntry entry = new AssertionEntry();
            AssertionType assertionType = AssertionType.valueOfName(assertion);

            if(type != null) {
                entry.setType(assertionType);
            } else {
                entry.setAssertionClass(assertion);
            }

            entry.setArgs(args);

            if(type.equalsIgnoreCase("condition")) {
                request.getConditions().add(entry);
            } else if(type.equalsIgnoreCase("assert")) {
                request.getAssertions().add(entry);
            }
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

    public enum Section {

        DEFAULT, API, REQUEST, FLOW
    }
}
