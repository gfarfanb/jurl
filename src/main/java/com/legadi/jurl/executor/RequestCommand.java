package com.legadi.jurl.executor;

import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isEmpty;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.JsonUtils.toJsonString;
import static com.legadi.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.jurl.common.ObjectsRegistry.findOrFail;
import static com.legadi.jurl.common.WriterUtils.appendToFile;
import static com.legadi.jurl.common.WriterUtils.writeFile;
import static java.util.logging.Level.FINE;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;
import java.util.stream.IntStream;

import com.legadi.jurl.common.ExecutionLevels;
import com.legadi.jurl.common.OutputPathBuilder;
import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.common.StringExpander;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.InvalidAssertionsFoundException;
import com.legadi.jurl.exception.RecursiveCommandException;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.exception.SkipExecutionException;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.HistoryEntry;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.RequestInput;
import com.legadi.jurl.model.ResponseEntry;
import com.legadi.jurl.model.StepEntry;
import com.legadi.jurl.options.OptionsReader;
import com.legadi.jurl.options.OptionsReader.OptionEntry;
import com.legadi.jurl.parser.RequestParser;

public class RequestCommand {

    private static final Logger LOGGER = Logger.getLogger(RequestCommand.class.getName());

    private static final int FIRST_EXECUTION = 0;

    private final OptionsReader optionsReader;
    private final ExecutionLevels executionLevels;

    public RequestCommand(String[] args) {
        this.optionsReader = new OptionsReader(args);
        this.executionLevels = new ExecutionLevels();

        optionsReader.registerAddOnOptions();
    }

    public void execute() {
        Settings settings = new Settings();

        executeOptions(settings, optionsReader.getOptionEntries());
        executeInputPath(settings, optionsReader.getRequestInputPath());
    }

    private void executeOptions(Settings settings, List<OptionEntry> optionEntries) {
        for(OptionEntry optionEntry : optionEntries) {
            boolean canContinue = optionEntry.getLeft().execute(
                settings, optionEntry.getRight()
            );

            if(!canContinue) {
                throw new SkipExecutionException();
            }
        }
    }

    private void executeInputPath(Settings settings, String requestInputPath) {
        if(isBlank(requestInputPath)) {
            throw new CommandException("Request input path is null or empty");
        }

        RequestParser<?> requestParser = findOrFail(RequestParser.class, settings.getRequestType());
        RequestInput<?> parsedRequestInput = requestParser.parseInput(settings, Paths.get(requestInputPath));

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, settings.getRequestType());
        Pair<String, RequestInput<?>> requestInput = modifier.appendAuthenticationIfExists(
                settings, parsedRequestInput, optionsReader.getOptionEntries());

        int times = settings.getTimes() > 0 ? settings.getTimes() : FIRST_EXECUTION;
        String inputName = requestInput.getLeft();
        boolean isExecutionAsFlow = requestInput.getRight().getFlows().containsKey(inputName);

        IntStream.range(0, times)
            .parallel()
            .forEach(index -> {
                if(isExecutionAsFlow) {
                    if(index == FIRST_EXECUTION) {
                        executionLevels.nextLevel();
                    }
                    LOGGER.fine("Executing flow -"
                        + " index=" + index
                        + " inputName=\"" + inputName + "\""
                        + " requestInputPath=\"" + requestInputPath + "\"");
                    processFlow(index, inputName, requestInputPath, requestInput.getRight(),
                        new StringExpander(settings.createForNextExecution()));
                } else {
                    LOGGER.fine("Executing request -"
                        + " index=" + index
                        + " inputName=\"" + inputName + "\""
                        + " requestInputPath=\"" + requestInputPath + "\"");
                    processRequest(index, inputName, requestInputPath, requestInput.getRight(),
                        new StringExpander(settings.createForNextExecution()));
                }
            });
    }

    private void processFlow(int index, String flowName, String requestInputPath,
            RequestInput<?> requestInput, StringExpander stringExpander) {
        if(isEmpty(requestInput.getFlows())) {
            throw new CommandException("No flows are defined in the request file: " + requestInputPath);
        }

        Settings settings = stringExpander.getSettings();
        List<StepEntry> steps = pickFlow(flowName, requestInput, settings);

        if(isEmpty(steps)) {
            throw new CommandException("No steps defined for the flow: "
                + steps + " - " + requestInputPath);
        }

        int stepIndex = 1;

        for(StepEntry step : steps) {
            Settings stepSettings = settings.createForStep();

            try {
                executeStep(stepSettings, step, requestInputPath, requestInput);
            } catch(RecursiveCommandException ex) {
                throw ex;
            } catch(CommandException | RequestException ex) {
                throw new CommandException(
                    "[" + requestInputPath + "/" + flowName + "]"
                    + (settings.getTimes() > 1 ? " index=" + index + " ": "")
                    + " step("+ stepIndex + "/" + steps.size() + ") "
                    + ex.getMessage());
            }

            stepIndex++;
        }
    }

    private List<StepEntry> pickFlow(String flowName, RequestInput<?> requestInput,
            Settings settings) {
        if(isNotBlank(flowName)) {
            return requestInput.getFlows().get(flowName);
        } else {
            flowName = requestInput.getFlows().keySet().stream().findFirst().get();
            return requestInput.getFlows().get(flowName);
        }
    }

    private void executeStep(Settings settings, StepEntry step, String requestInputPath,
            RequestInput<?> requestInput) {
        executeOptions(settings, step.getOptions());

        if(isNotBlank(step.getRequestInputPath())) {
            executeInputPath(settings, step.getRequestInputPath());
        } else {
            executeInputPath(settings, requestInputPath);
        }
    }

    private void processRequest(int index, String requestName, String requestInputPath,
            RequestInput<?> requestInput, StringExpander stringExpander) {
        if(isEmpty(requestInput.getRequests())) {
            throw new CommandException("No requests are defined in the request file: " + requestInputPath);
        }

        Settings settings = stringExpander.getSettings();
        RequestEntry<? extends MockEntry> request = requestInput.getRequests().get(requestName);

        if(request == null) {
            throw new CommandException("No request defined for name: "
                + requestName + " - " + requestInputPath);
        }
        if(index == FIRST_EXECUTION && executionLevels.wasExecuted(requestInputPath, requestName)) {
            throw new RecursiveCommandException("Request input was already processed: "
                + requestInputPath + "/" + requestName + " - " + executionLevels.getTrace());
        }

        executeOptions(settings, request.getOptions());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, settings.getRequestType());
        modifier.mergeHeader(requestInput.getApi(), request);

        try {
            executeRequest(stringExpander, requestInputPath, requestInput.getApi(), request);
        } catch(CommandException | RequestException ex) {
            throw new CommandException(
                "[" + requestInputPath + "/" + requestName + "] "
                + (settings.getTimes() > 1 ? " index=" + index + " ": "")
                + ex.getMessage());
        }
    }

    private void executeRequest(StringExpander stringExpander, String requestInputPath,
            RequestEntry<? extends MockEntry> api, RequestEntry<? extends MockEntry> request) {
        Settings settings = stringExpander.getSettings();
        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, settings.getRequestType());
        RequestExecutor<?, ?> executor = findByNameOrFail(RequestExecutor.class, settings.getRequestType());
        ResponseProcessor<?, ?> processor = findByNameOrFail(ResponseProcessor.class, settings.getRequestType());
        Optional<AssertionResult> conditionsResult = executor.accepts(settings, request);

        if(conditionsResult.isPresent() && !conditionsResult.get().isPassed()) {
            LOGGER.info("Request skipped - inputFile=\"" + requestInputPath + "\""
                    + " request=\"" + request.getName() + "\""
                    + " environment=\"" + settings.getEnvironment() + "\""
                    + " executionTag=\"" + settings.getExecutionTag() + "\"");
            conditionsResult.get().getFailedMessages().forEach(LOGGER::info);
            LOGGER.info("");
            return;
        }

        modifier.mergeAPI(settings, api, request);

        if(isNotBlank(settings.getMergeBodyUsingType())) {
            modifier.mergeBody(settings, requestInputPath, request);
        }

        if(isNotBlank(settings.getOverrideRequestFilePath())) {
            modifier.overrideRequest(settings, request, settings.getOverrideRequestFilePath());
        }

        long beginTime = System.nanoTime();
        ResponseEntry response = executor.execute(settings, requestInputPath, request);
        long endTime = System.nanoTime();

        HistoryEntry historyEntry = new HistoryEntry();
        historyEntry.setCurl(response.getCurlCommand());
        historyEntry.setResult(response.getResult());
        historyEntry.setRequestPath(requestInputPath);
        historyEntry.setRequestName(request.getName());
        historyEntry.setEnvironment(settings.getEnvironment());
        historyEntry.setWorkspacePath(settings.getWorkspacePath().toString());
        historyEntry.setTimestamp(settings.getTimestamp().toEpochSecond(OffsetDateTime.now().getOffset()));
        historyEntry.setExecutionTag(settings.getExecutionTag());
        historyEntry.setNanoTime(endTime - beginTime);
        historyEntry.setDetails(processor.getDetails(response));

        Optional<AssertionResult> assertionResult = processor.process(settings, request, response);

        if(assertionResult.isPresent()) {
            historyEntry.setAssertions(assertionResult.get().getAssertions());
            historyEntry.setFailures(assertionResult.get().getFailures());

            if(!assertionResult.get().isPassed()) {
                LOGGER.warning("Request failed -"
                    + " inputFile=\"" + requestInputPath + "\""
                    + " request=\"" + request.getName() + "\""
                    + " result=\"" + response.getResult() + "\""
                    + " environment=\"" + settings.getEnvironment() + "\""
                    + " executionTag=\"" + settings.getExecutionTag() + "\""
                    + " time(nano)=" + historyEntry.getNanoTime());
                assertionResult.get().getFailedMessages().forEach(LOGGER::warning);
            } else {
                LOGGER.info("Request successful -"
                    + " inputFile=\"" + requestInputPath + "\""
                    + " request=\"" + request.getName() + "\""
                    + " result=\"" + response.getResult() + "\""
                    + " environment=\"" + settings.getEnvironment() + "\""
                    + " executionTag=\"" + settings.getExecutionTag() + "\""
                    + " time(nano)=" + historyEntry.getNanoTime());
            }
        } else {
            LOGGER.info("Request successful -"
                    + " inputFile=\"" + requestInputPath + "\""
                    + " request=\"" + request.getName() + "\""
                    + " result=\"" + response.getResult() + "\""
                    + " environment=\"" + settings.getEnvironment() + "\""
                    + " executionTag=\"" + settings.getExecutionTag() + "\""
                    + " time(nano)=" + historyEntry.getNanoTime());
        }

        LOGGER.info("");

        saveHistory(settings, historyEntry, requestInputPath, request.getName());

        if(assertionResult.isPresent() && !assertionResult.get().isPassed()) {
            throw new InvalidAssertionsFoundException();
        }

        if(settings.isOpenOutputInEditor()) {
            openEditorCommand(stringExpander);
        }
    }

    private void saveHistory(Settings settings, HistoryEntry entry, String requestPath, String requestName) {
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setRequestPath(requestPath)
            .setRequestName(requestName)
            .setFilename(settings.getTimestamp().toLocalDate().toString())
            .setExtension("history.json");
        Path historyPath = pathBuilder.buildHistoryPath();
        File historyFile = historyPath.toFile();

        if(historyFile.exists()) {
            appendToFile(historyFile, historyFile.length() - 1, ",", toJsonString(entry), "]");
        } else {
            writeFile(historyPath, "[", toJsonString(entry), "]");
        }
    }

    private void openEditorCommand(StringExpander stringExpander) {
        Settings settings = stringExpander.getSettings();

        if(isBlank(settings.getOpenEditorCommand())) {
            LOGGER.fine("'openEditorCommand' is null or empty");
            return;
        }

        try {
            String cmd = stringExpander.replaceAllInContent(settings.getOpenEditorCommand());
            LOGGER.fine("Executing command: " + cmd);
            Process process = Runtime.getRuntime().exec(cmd);
            LOGGER.info("Command status [" + process.waitFor() + "]: " + cmd);
        } catch(Exception ex) {
            LOGGER.log(FINE, "Error on opening editor command - " + ex.getMessage(), ex);
            ex.printStackTrace();
        }
    }
}
