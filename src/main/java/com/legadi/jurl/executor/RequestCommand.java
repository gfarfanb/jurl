package com.legadi.jurl.executor;

import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isEmpty;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.JsonUtils.toJsonString;
import static com.legadi.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.jurl.common.ObjectsRegistry.findOrFail;
import static com.legadi.jurl.common.WriterUtils.appendToFile;
import static com.legadi.jurl.common.WriterUtils.writeFile;
import static com.legadi.jurl.model.ExecutionStatus.FAILED;
import static com.legadi.jurl.model.ExecutionStatus.SKIPPED;
import static com.legadi.jurl.model.ExecutionStatus.SUCCESSFUL;
import static com.legadi.jurl.model.RequestBehaviour.CURL_ONLY;
import static com.legadi.jurl.model.RequestBehaviour.PRINT_ONLY;
import static java.util.logging.Level.FINE;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.OffsetDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
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
import com.legadi.jurl.model.AuthenticationRequest;
import com.legadi.jurl.model.ExecutionIndex;
import com.legadi.jurl.model.ExecutionStats;
import com.legadi.jurl.model.ExecutionStatus;
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

    private static final Set<AuthenticationRequest<?>> EXECUTED_AUTH_REQUESTS = new HashSet<>();
    private static final int FIRST_EXECUTION = 0;
    private static final int AUTH_AND_REQUEST_EXECUTIONS = 2;
    private static final int ONLY_REQUEST_EXECUTIONS = 1;

    private final OptionsReader optionsReader;
    private final ExecutionLevels executionLevels;
    private final Lock lock;

    public RequestCommand(String[] args) {
        this.optionsReader = new OptionsReader(args);
        this.executionLevels = new ExecutionLevels();
        this.lock = new ReentrantLock();
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

    private ExecutionStatus executeInputPath(Settings settings, String requestInputPath) {
        if(isBlank(requestInputPath)) {
            throw new CommandException("Request input path is null or empty");
        }

        RequestParser<?> requestParser = findOrFail(RequestParser.class, settings.getRequestType());
        RequestInput<?> requestInput = requestParser.parseInput(settings, Paths.get(requestInputPath));

        int times = settings.getTimes() > 0 ? settings.getTimes() : FIRST_EXECUTION;
        String inputName = isNotBlank(settings.getInputName())
            ? settings.getInputName()
            : requestInput.getDefaultRequest();
        boolean isExecutionAsFlow = requestInput.getFlows().containsKey(inputName);
        ExecutionStats stats = new ExecutionStats(times);
        AtomicReference<String> inputNameCarrier = new AtomicReference<>();

        try {
            IntStream.range(0, times)
                .parallel()
                .mapToObj(index -> new ExecutionIndex(index, index + 1, times))
                .forEach(index -> {
                    if(isExecutionAsFlow) {
                        if(index.getIndex() == FIRST_EXECUTION) {
                            executionLevels.nextLevel();
                        }

                        Pair<String, ExecutionStats> flowStats = processFlow(index, inputName, requestInputPath, requestInput,
                            settings.createForNextExecution());

                        inputNameCarrier.set(flowStats.getLeft());
                        stats.count(flowStats.getRight().computeStatus());
                    } else {
                        Pair<String, ExecutionStats> requestStats = processRequest(index, inputName, requestInputPath, requestInput,
                            settings.createForNextExecution());

                        inputNameCarrier.set(requestStats.getLeft());
                        stats.count(requestStats.getRight().computeStatus());
                    }
                });

            return stats.computeStatus();
        } finally {
            if(times > 1) {
                LOGGER.info("Execution completed -"
                    + " inputFile=\"" + requestInputPath + "\""
                    + " inputName=\"" + inputNameCarrier.get() + "\""
                    + " environment=\"" + settings.getEnvironment() + "\""
                    + " executions=" + stats.getExecutions()
                    + " stats=" + stats);
            }
        }
    }

    private Pair<String, ExecutionStats> processFlow(ExecutionIndex index, String flowName, String requestInputPath,
            RequestInput<?> requestInput, Settings settings) {
        if(isEmpty(requestInput.getFlows())) {
            throw new CommandException("No flows are defined in the request file: " + requestInputPath);
        }

        Pair<String, List<StepEntry>> flowDefinition = pickFlow(flowName, requestInput, settings);
        List<StepEntry> steps = flowDefinition.getRight();
        flowName = flowDefinition.getLeft();

        if(isEmpty(steps)) {
            throw new CommandException("No steps defined for the flow: "
                + flowName + " - " + requestInputPath);
        }

        LOGGER.fine("Executing flow -"
            + " index=" + index
            + " flowName=\"" + flowName + "\""
            + " requestInputPath=\"" + requestInputPath + "\"");

        ExecutionStats stats = new ExecutionStats(steps.size());
        int stepIndex = 1;

        for(StepEntry step : steps) {
            Settings stepSettings = settings.createForStep();

            try {
                ExecutionStatus status = executeStep(stepSettings, step, requestInputPath, requestInput);
                stats.count(status);
            } catch(RecursiveCommandException ex) {
                throw ex;
            } catch(CommandException | RequestException ex) {
                throw new CommandException(
                    "[" + requestInputPath + "/" + flowName + "]"
                    + " index=" + index
                    + " step("+ stepIndex + "/" + steps.size() + ") "
                    + " - " + ex.getMessage());
            }

            stepIndex++;
        }

        LOGGER.info("Steps completed -"
            + " index=" + index
            + " inputFile=\"" + requestInputPath + "\""
            + " flowName=\"" + flowName + "\""
            + " environment=\"" + settings.getEnvironment() + "\""
            + " steps=" + stats.getExecutions()
            + " stats=" + stats);

        return new Pair<>(flowName, stats);
    }

    private Pair<String, List<StepEntry>> pickFlow(String flowName, RequestInput<?> requestInput,
            Settings settings) {
        if(isBlank(flowName)) {
            flowName = requestInput.getFlows().keySet().stream().findFirst().get();
        }

        return new Pair<>(flowName, requestInput.getFlows().get(flowName));
    }

    private ExecutionStatus executeStep(Settings settings, StepEntry step, String requestInputPath,
            RequestInput<?> requestInput) {
        executeOptions(settings, step.getOptions());

        if(isNotBlank(step.getRequestInputPath())) {
            return executeInputPath(settings, step.getRequestInputPath());
        } else {
            return executeInputPath(settings, requestInputPath);
        }
    }

    private Pair<String, ExecutionStats> processRequest(ExecutionIndex index, String requestName, String requestInputPath,
            RequestInput<?> requestInput, Settings settings) {
        if(isEmpty(requestInput.getRequests())) {
            throw new CommandException("No requests are defined in the request file: " + requestInputPath);
        }

        StringExpander stringExpander = new StringExpander(settings);
        Pair<String, RequestEntry<? extends MockEntry>> requestDefinition = pickRequest(requestName, requestInput, settings);
        RequestEntry<? extends MockEntry> request = requestDefinition.getRight();
        requestName = requestDefinition.getLeft();

        if(request == null) {
            throw new CommandException("No request defined for name: "
                + requestName + " - " + requestInputPath);
        }

        if(index.getIndex() == FIRST_EXECUTION && executionLevels.wasExecuted(requestInputPath, requestName)) {
            throw new RecursiveCommandException("Request input was already processed: "
                + requestInputPath + "/" + requestName + " - " + executionLevels.getTrace());
        }

        Optional<ExecutionStatus> authStatus = processAuthentication(requestName,
                requestInputPath, requestInput, settings.createForNextExecution());
        ExecutionStats stats;

        if(authStatus.isPresent()) {
            stats = new ExecutionStats(AUTH_AND_REQUEST_EXECUTIONS);
            stats.count(authStatus.get());
        } else {
            stats = new ExecutionStats(ONLY_REQUEST_EXECUTIONS);
        }

        LOGGER.fine("Executing request -"
            + " index=" + index
            + " requestName=\"" + requestName + "\""
            + " requestInputPath=\"" + requestInputPath + "\"");

        executeOptions(settings, request.getOptions());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, settings.getRequestType());
        modifier.mergeHeader(requestInput.getApi(), request);

        try {
            ExecutionStatus status = executeRequest(index, stringExpander,
                requestInputPath, requestInput.getApi(), request);

            stats.count(status);

            return new Pair<>(requestName, stats);
        } catch(CommandException | RequestException ex) {
            throw new CommandException(
                "[" + requestInputPath + "/" + requestName + "] "
                + " index=" + index
                + " - " + ex.getMessage());
        }
    }

    private Pair<String, RequestEntry<? extends MockEntry>> pickRequest(String requestName, RequestInput<?> requestInput,
            Settings settings) {
        if(isBlank(requestName)) {
            requestName = requestInput.getRequests().keySet().stream().findFirst().get();
        }

        return new Pair<>(requestName, requestInput.getRequests().get(requestName));
    }

    @SuppressWarnings("unchecked")
    private Optional<ExecutionStatus> processAuthentication(String requestName,
            String requestInputPath, RequestInput<?> requestInput, Settings settings) {
        lock.lock();

        try {
            if(settings.isSkipAuthentication()) {
                return Optional.empty();
            }

            RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, settings.getRequestType());
            Optional<?> authRequestCarrier = modifier.getAuthenticationIfExists(requestName,
                requestInputPath, requestInput, settings, optionsReader.getOptionEntries());

            if(!authRequestCarrier.isPresent()) {
                return Optional.empty();
            }

            AuthenticationRequest<? extends RequestEntry<? extends MockEntry>> authRequest =
                (AuthenticationRequest<? extends RequestEntry<? extends MockEntry>>) authRequestCarrier.get();

            if(EXECUTED_AUTH_REQUESTS.contains(authRequest)) {
                return Optional.empty();
            } else {
                EXECUTED_AUTH_REQUESTS.add(authRequest);
            }

            ExecutionIndex index = new ExecutionIndex(0, 1, 1);

            LOGGER.fine("Executing authentication request -"
                + " index=" + index
                + " authName=\"" + authRequest.getAuthRequestName() + "\""
                + " requestInputPath=\"" + authRequest.getAuthRequestInputPath() + "\"");

            executeOptions(settings, authRequest.getAuthOptions());
            modifier.mergeHeader(authRequest.getAuthApi(), authRequest.getAuthRequest());

            try {
                return Optional.of(executeRequest(index, new StringExpander(settings),
                    authRequest.getAuthRequestInputPath(), authRequest.getAuthApi(), authRequest.getAuthRequest()));
            } catch(CommandException | RequestException ex) {
                throw new CommandException(
                    "[" + authRequest.getAuthRequestInputPath() + "/" + authRequest.getAuthRequestName() + "] "
                    + " index=" + index
                    + " - " + ex.getMessage());
            }
        } finally {
            lock.unlock();
        }
    }

    private ExecutionStatus executeRequest(ExecutionIndex index, StringExpander stringExpander, String requestInputPath,
            RequestEntry<? extends MockEntry> api, RequestEntry<? extends MockEntry> request) {
        Settings settings = stringExpander.getSettings();
        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, settings.getRequestType());
        RequestExecutor<?, ?> executor = findByNameOrFail(RequestExecutor.class, settings.getRequestType());
        ResponseProcessor<?, ?> processor = findByNameOrFail(ResponseProcessor.class, settings.getRequestType());
        Optional<AssertionResult> conditionsResult = executor.accepts(settings, request);

        if(!isPrintOnly(settings)
                && conditionsResult.isPresent()
                && !conditionsResult.get().isPassed()) {
            LOGGER.info(executionCompletedMessage(settings, index, requestInputPath,
                request, null, SKIPPED, null));
            conditionsResult.get().getFailedMessages().forEach(LOGGER::info);
            LOGGER.info("");
            return FAILED;
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
        historyEntry.setIndex(index.toString());
        historyEntry.setDetails(processor.getDetails(response));

        Optional<AssertionResult> assertionResult = processor.process(settings, request, response);
        ExecutionStatus status = SUCCESSFUL;

        if(assertionResult.isPresent()) {
            historyEntry.setAssertions(assertionResult.get().getAssertions());
            historyEntry.setFailures(assertionResult.get().getFailures());

            if(!assertionResult.get().isPassed()) {
                status = FAILED;
                LOGGER.warning(executionCompletedMessage(settings, index, requestInputPath,
                    request, response, status, historyEntry));
                assertionResult.get().getFailedMessages().forEach(LOGGER::warning);
            } else {
                LOGGER.info(executionCompletedMessage(settings, index, requestInputPath,
                    request, response, status, historyEntry));
            }
        } else if(!isPrintOnly(settings)) {
            LOGGER.info(executionCompletedMessage(settings, index, requestInputPath,
                request, response, status, historyEntry));
        }

        LOGGER.info("");

        saveHistory(settings, historyEntry, requestInputPath, request.getName());

        if(assertionResult.isPresent() && !assertionResult.get().isPassed()) {
            throw new InvalidAssertionsFoundException();
        }

        if(settings.isOpenOutputInEditor()) {
            openEditorCommand(stringExpander);
        }

        return status;
    }

    private void saveHistory(Settings settings, HistoryEntry entry, String requestPath, String requestName) {
        lock.lock();

        try {
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
        } finally {
            lock.unlock();
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
            LOGGER.severe("Error on opening editor command - " + ex.getMessage());
            LOGGER.log(FINE, "Unable to run editor command", ex);
        }
    }

    private boolean isPrintOnly(Settings settings) {
        return settings.getRequestBehaviour() == CURL_ONLY
            || settings.getRequestBehaviour() == PRINT_ONLY;
    }

    private String executionCompletedMessage(Settings settings, ExecutionIndex index,
            String inputPath, RequestEntry<?> request, ResponseEntry response,
            ExecutionStatus status, HistoryEntry historyEntry) {
        return "Request completed -"
            + " status=" + status
            + " inputFile=\"" + inputPath + "\""
            + " requestName=\"" + request.getName() + "\""
            + (response != null ? " result=\"" + response.getResult() + "\"" : "")
            + " environment=\"" + settings.getEnvironment() + "\""
            + " index=" + index
            + " executionTag=\"" + settings.getExecutionTag() + "\""
            + (historyEntry != null  ? " time(nano)=" + historyEntry.getNanoTime() : "");
    }
}
