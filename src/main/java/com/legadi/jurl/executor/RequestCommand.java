package com.legadi.jurl.executor;

import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isEmpty;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.jurl.common.JsonUtils.toJsonString;
import static com.legadi.jurl.common.LoaderUtils.loadCredentials;
import static com.legadi.jurl.common.RequestUtils.mergeRequestHeader;
import static com.legadi.jurl.common.WriterUtils.appendToFile;
import static com.legadi.jurl.common.WriterUtils.writeFile;
import static com.legadi.jurl.executor.RequestHandlersRegistry.findExecutorByRequestType;
import static com.legadi.jurl.executor.RequestHandlersRegistry.findProcessorByRequestType;
import static com.legadi.jurl.parser.RequestParserRegistry.findByRequestType;

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
        executeInputPath(settings, optionsReader.getRequestInputPath(), true);
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

    private void executeInputPath(Settings settings, String requestInputPath, boolean isMainInput) {
        if(isBlank(requestInputPath)) {
            throw new CommandException("Request input path is null or empty");
        }

        RequestParser<?> requestParser = findByRequestType(settings.getRequestType());
        RequestInput<?> requestInput = requestParser.parseInput(settings, Paths.get(requestInputPath));

        executeInput(settings, requestInputPath, requestInput, isMainInput);
    }

    private void executeInput(Settings settings, String requestInputPath,
            RequestInput<?> requestInput, boolean isMainInput) {
        loadConfig(requestInput, settings, isMainInput);

        int times = settings.getTimes() > 0 ? settings.getTimes() : FIRST_EXECUTION;

        IntStream.range(0, times)
            .parallel()
            .forEach(index -> {
                if(settings.isExecutionAsFlow()) {
                    if(index == FIRST_EXECUTION) {
                        executionLevels.nextLevel();
                    }
                    processFlow(index, requestInputPath, requestInput,
                        new StringExpander(settings.createForNextExecution()));
                } else {
                    processRequest(index, requestInputPath, requestInput,
                        new StringExpander(settings.createForNextExecution()));
                }
            });
    }

    private void loadConfig(RequestInput<?> requestInput, Settings settings, boolean isMainInput) {
        String environment = settings.getEnvironment();
        Path configPath = settings.getConfigFilePath();
        Path credentialsPath = settings.getCredentialsFilePath();

        Settings.mergeProperties(environment, loadJsonProperties(configPath));
        Settings.mergeCredentials(environment, loadCredentials(credentialsPath));

        if(isMainInput) {
            Settings.mergeProperties(environment, requestInput.getConfig());
        }
    }

    private void processFlow(int index, String requestInputPath, RequestInput<?> requestInput,
            StringExpander stringExpander) {
        if(isEmpty(requestInput.getFlows())) {
            throw new CommandException("No flows are defined in the request file: " + requestInputPath);
        }

        Settings settings = stringExpander.getSettings();
        Pair<String, List<StepEntry>> flowDef = pickFlow(requestInput, settings);

        if(isEmpty(flowDef.getRight())) {
            throw new CommandException("No steps defined for the flow: "
                + flowDef.getLeft() + " - " + requestInputPath);
        }

        List<StepEntry> steps = flowDef.getRight();
        int stepIndex = 1;

        for(StepEntry step : steps) {
            Settings stepSettings = new Settings(settings.getEnvironment());
            String name = flowDef.getLeft();

            try {
                executeStep(stepSettings, step, requestInputPath, requestInput);
            } catch(RecursiveCommandException ex) {
                throw ex;
            } catch(CommandException | RequestException ex) {
                throw new CommandException(
                    "[" + requestInputPath + "/" + name + "]"
                    + (settings.getTimes() > 1 ? " index=" + index + " ": "")
                    + " step("+ stepIndex + "/" + steps.size() + ") "
                    + ex.getMessage());
            }

            stepIndex++;
        }
    }

    private Pair<String, List<StepEntry>> pickFlow(RequestInput<?> requestInput, Settings settings) {
        String name = isNotBlank(settings.getInputName())
            ? settings.getInputName()
            : requestInput.getDefaultFlow();

        if(isNotBlank(name)) {
            return new Pair<>(name, requestInput.getFlows().get(name));
        } else {
            String flowName = requestInput.getFlows().keySet().stream().findFirst().get();
            return new Pair<>(flowName, requestInput.getFlows().get(flowName));
        }
    }

    private void executeStep(Settings settings, StepEntry step, String requestInputPath,
            RequestInput<?> requestInput) {
        executeOptions(settings, step.getOptions());

        if(isNotBlank(step.getRequestInputPath())) {
            executeInputPath(settings, step.getRequestInputPath(), false);
        } else {
            executeInput(settings, requestInputPath, requestInput, false);
        }
    }

    private void processRequest(int index, String requestInputPath, RequestInput<?> requestInput,
            StringExpander stringExpander) {
        if(isEmpty(requestInput.getRequests())) {
            throw new CommandException("No requests are defined in the request file: " + requestInputPath);
        }

        Settings settings = stringExpander.getSettings();
        Pair<String, RequestEntry<? extends MockEntry>> requestDef = pickRequest(requestInput, settings);

        if(requestDef.getRight() == null) {
            throw new CommandException("No request defined for name: "
                + requestDef.getLeft() + " - " + requestInputPath);
        }
        if(index == FIRST_EXECUTION && executionLevels.wasExecuted(requestInputPath, requestDef.getLeft())) {
            throw new RecursiveCommandException("Request input was already processed: "
                + requestInputPath + "/" + requestDef.getLeft() + " - " + executionLevels.getTrace());
        }

        RequestEntry<? extends MockEntry> request = requestDef.getRight();

        if(requestInput.getApi() != null) {
            mergeRequestHeader(requestInput.getApi(), request);
        }

        try {
            executeRequest(settings, requestInputPath, requestInput.getApi(), request);
        } catch(CommandException | RequestException ex) {
            throw new CommandException(
                "[" + requestInputPath + "/" + requestDef.getLeft() + "] "
                + (settings.getTimes() > 1 ? " index=" + index + " ": "")
                + ex.getMessage());
        }
    }

    private Pair<String, RequestEntry<? extends MockEntry>> pickRequest(
            RequestInput<?> requestInput, Settings settings) {
        String name = isNotBlank(settings.getInputName())
            ? settings.getInputName()
            : requestInput.getDefaultRequest();

        if(isNotBlank(name)) {
            return new Pair<>(name, requestInput.getRequests().get(name));
        } else {
            String requestName = requestInput.getRequests().keySet().stream().findFirst().get();
            return new Pair<>(requestName, requestInput.getRequests().get(requestName));
        }
    }

    private void executeRequest(Settings settings, String requestInputPath,
            RequestEntry<? extends MockEntry> api, RequestEntry<? extends MockEntry> request) {
        RequestExecutor<?, ?> executor = findExecutorByRequestType(settings.getRequestType());
        ResponseProcessor<?, ?> processor = findProcessorByRequestType(settings.getRequestType());
        Optional<AssertionResult> conditionsResult = executor.accepts(settings, request);

        if(conditionsResult.isPresent() && !conditionsResult.get().isPassed()) {
            LOGGER.info("Request skipped - inputFile=" + requestInputPath + " request=" + request.getName());
            conditionsResult.get().getFailedMessages().forEach(LOGGER::info);
            return;
        }

        if(api != null) {
            executor.mergeAPI(settings, api, request);
        }

        if(isNotBlank(settings.getMergeBodyUsingType())) {
            executor.mergeBody(settings, requestInputPath, request);
        }

        if(isNotBlank(settings.getOverrideRequestFilePath())) {
            executor.overrideRequest(settings, request, settings.getOverrideRequestFilePath());
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
        historyEntry.setTimestamp(settings.getTimestamp().toEpochSecond(OffsetDateTime.now().getOffset()));
        historyEntry.setExecutionTag(settings.getExecutionTag());
        historyEntry.setNanoTime(endTime - beginTime);

        Optional<AssertionResult> assertionResult = processor.process(settings, request, response);

        if(assertionResult.isPresent()) {
            historyEntry.setAssertions(assertionResult.get().getAssertions());
            historyEntry.setFailures(assertionResult.get().getFailures());

            if(!assertionResult.get().isPassed()) {
                LOGGER.warning("Request failed - inputFile=" + requestInputPath
                    + " request=" + request.getName()
                    + " time(nano)=" + historyEntry.getNanoTime());
                assertionResult.get().getFailedMessages().forEach(LOGGER::warning);
            } else {
                LOGGER.info("Request successful - inputFile=" + requestInputPath
                    + " request=" + request.getName()
                    + " time(nano)=" + historyEntry.getNanoTime());
            }
        } else {
            LOGGER.info("Request successful - inputFile=" + requestInputPath
                    + " request=" + request.getName()
                    + " time(nano)=" + historyEntry.getNanoTime());
        }

        saveHistory(settings, historyEntry, requestInputPath, request.getName());

        if(assertionResult.isPresent() && !assertionResult.get().isPassed()) {
            throw new InvalidAssertionsFoundException();
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
}
