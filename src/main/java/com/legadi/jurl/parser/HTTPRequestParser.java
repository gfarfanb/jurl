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

    private final Pattern defaultRequestPattern = Pattern.compile("^(?i)@default-request[ ]*=(.*)");
    private final Pattern defaultFlowPattern = Pattern.compile("^(?i)@default-flow[ ]*=(.*)");
    private final Pattern sectionPattern = Pattern.compile("^(?i)###[ ]*\\[(api|request|flow)\\](.*)");
    private final Pattern fieldPattern = Pattern.compile("^@([\\w]+)[ ]*=(.*)");
    private final Pattern urlMethodPattern = Pattern.compile("^(?i)([\\w]+)?[ ]*(http|https):\\/\\/(.*)");
    private final Pattern headerPattern = Pattern.compile("^([\\w-]+): (.*)");
    private final Pattern queryParamPattern = Pattern.compile("^[&]*([\\w:.-_@]+)=(.*)");
    private final Pattern fileFieldPattern = Pattern.compile("^(?i)file @([\\w:.-_@]+)[ ]*=(.*)");
    private final Pattern fileFormPattern = Pattern.compile("^(?i)file ([\\w:.-_@]+)[ ]*=(.*)");
    private final Pattern mockFieldPattern = Pattern.compile("^(?i)mock @([\\w:.-_@]+)[ ]*=(.*)");
    private final Pattern mockHeaderPattern = Pattern.compile("^(?i)mock ([\\w-]+): (.*)");
    private final Pattern outputPattern = Pattern.compile("^(?i)output ([\\w:.-_@]+)[ ]*=(.*)");
    private final Pattern assertionPattern = Pattern.compile("^(?i)assert ([\\w:.-_@]+) (.*)");
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

                if(addDefaultRequest(requestInput, line)) {
                    continue;
                }
                if(addDefaultFlow(requestInput, line)) {
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

        if(addField(fieldPattern, REQUEST_FIELDS, () -> request, line)) {
            return;
        }
        if(addURLMethod(request, line)) {
            return;
        }
        if(addHeader(request, line)) {
            return;
        }
        if(addQueryParam(request, line)) {
            return;
        }

        Supplier<HTTPRequestFileEntry> fileSupplier = () -> {
            if(request.getRequestFile() == null) {
                request.setRequestFile(new HTTPRequestFileEntry());
            }
            return request.getRequestFile();
        };
        if(addField(fileFieldPattern, REQUEST_FILE_FIELDS, fileSupplier, line)) {
            return;
        }
        if(addFileForm(fileSupplier, line)) {
            return;
        }

        Supplier<HTTPMockEntry> mockSupplier = () -> {
            if(request.getMockDefinition() == null) {
                request.setMockDefinition(new HTTPMockEntry());
            }
            return request.getMockDefinition();
        };
        if(addField(mockFieldPattern, MOCK_FIELDS, mockSupplier, line)) {
            return;
        }
        if(addMockHeader(mockSupplier, line)) {
            return;
        }

        if(addOutputMapping(request, line)) {
            return;
        }
        if(addAssertion(request, line)) {
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

    private boolean addHeader(HTTPRequestEntry request, String line) {
        Matcher matcher = headerPattern.matcher(line);

        if(matcher.find()) {
            String header = trim(matcher.group(1));
            String value = trim(matcher.group(2));
            request.getHeaders().put(header, value);
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

    private boolean addField(Pattern pattern, Map<String, Field> fields,
            Supplier<?> objectSupplier, String line) throws IllegalAccessException {
        Matcher matcher = pattern.matcher(line);

        if(matcher.find()) {
            String fieldName = trim(matcher.group(1));
            String value = trim(matcher.group(2));
            Field field = fields.get(fieldName);
            if(field == null) {
                throw new CommandException("Field not defined in " + objectSupplier.get().getClass()
                    + ": " + fieldName);
            }
            field.setAccessible(true);
            field.set(objectSupplier.get(), value);
            return true;
        } else {
            return false;
        }
    }

    private boolean addFileForm(Supplier<HTTPRequestFileEntry> fileSupplier, String line) {
        Matcher matcher = fileFormPattern.matcher(line);

        if(matcher.find()) {
            String param = trim(matcher.group(1));
            String value = trim(matcher.group(2));
            fileSupplier.get().getFormData().put(param, value);
            return true;
        } else {
            return false;
        }
    }

    private boolean addMockHeader(Supplier<HTTPMockEntry> mockSupplier, String line) {
        Matcher matcher = mockHeaderPattern.matcher(line);

        if(matcher.find()) {
            String header = trim(matcher.group(1));
            String value = trim(matcher.group(2));
            mockSupplier.get().getResponseHeaders().put(header, value);
            return true;
        } else {
            return false;
        }
    }

    private boolean addOutputMapping(HTTPRequestEntry request, String line) {
        Matcher matcher = outputPattern.matcher(line);

        if(matcher.find()) {
            String param = trim(matcher.group(1));
            String value = trim(matcher.group(2));
            request.getOutputMappings().put(param, value);
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

    public enum Section {

        DEFAULT, API, REQUEST, FLOW
    }
}
