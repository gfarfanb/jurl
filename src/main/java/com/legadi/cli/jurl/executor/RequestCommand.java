package com.legadi.cli.jurl.executor;

import static com.legadi.cli.jurl.common.Command.exec;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isEmpty;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.JsonUtils.toJsonString;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findOrFail;
import static com.legadi.cli.jurl.common.WriterUtils.appendToFile;
import static com.legadi.cli.jurl.common.WriterUtils.writeFile;
import static com.legadi.cli.jurl.model.ExecutionStatus.FAILED;
import static com.legadi.cli.jurl.model.ExecutionStatus.SKIPPED;
import static com.legadi.cli.jurl.model.ExecutionStatus.SUCCESSFUL;
import static java.util.logging.Level.INFO;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;
import java.util.stream.IntStream;

import com.legadi.cli.jurl.common.ExecutionStats;
import com.legadi.cli.jurl.common.ExecutionTrace;
import com.legadi.cli.jurl.common.InputNameResolver;
import com.legadi.cli.jurl.common.OutputPathBuilder;
import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.StepTag;
import com.legadi.cli.jurl.common.StringExpander;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.exception.InvalidAssertionsFoundException;
import com.legadi.cli.jurl.exception.RecursiveCommandException;
import com.legadi.cli.jurl.exception.RequestException;
import com.legadi.cli.jurl.exception.SkipExecutionException;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.ExecutionIndex;
import com.legadi.cli.jurl.model.ExecutionStatus;
import com.legadi.cli.jurl.model.FlowEntry;
import com.legadi.cli.jurl.model.HistoryEntry;
import com.legadi.cli.jurl.model.MockEntry;
import com.legadi.cli.jurl.model.RequestEntry;
import com.legadi.cli.jurl.model.RequestInput;
import com.legadi.cli.jurl.model.ResponseEntry;
import com.legadi.cli.jurl.model.StepEntry;
import com.legadi.cli.jurl.options.OptionsReader;
import com.legadi.cli.jurl.options.OptionsReader.OptionEntry;
import com.legadi.cli.jurl.parser.RequestParser;

public class RequestCommand {

    private static final Logger LOGGER = Logger.getLogger(RequestCommand.class.getName());

    private static final int EXECUTE_ONCE = 1;
    private static final int AUTH_AND_REQUEST_EXECUTIONS = 2;
    private static final int ONLY_REQUEST_EXECUTIONS = 1;

    private final OptionsReader optionsReader;
    private final Lock lock;

    public RequestCommand(String[] args) {
        this.optionsReader = new OptionsReader(args);
        this.lock = new ReentrantLock();
    }

    public void execute() {
        Settings settings = new Settings();

        executeOptions(settings, optionsReader.getOptionEntries());
        executeInputPath(new ExecutionTrace(settings), settings, null, optionsReader.getRequestInputPath(), null);
    }

    public void executeOptions(Settings settings, List<OptionEntry> optionEntries) {
        for(OptionEntry optionEntry : optionEntries) {
            boolean canContinue = optionEntry.getLeft().execute(
                settings, optionEntry.getRight()
            );

            if(!canContinue) {
                throw new SkipExecutionException();
            }
        }
    }

    public void executeOptionsNoSkip(Settings settings, List<OptionEntry> optionEntries) {
        for(OptionEntry optionEntry : optionEntries) {
            optionEntry.getLeft().execute(
                settings, optionEntry.getRight()
            );
        }
    }

    private ExecutionStatus executeInputPath(ExecutionTrace trace, Settings settings,
            RequestInput<?> parentRequestInput, String requestInputPath, StepTag stepTag) {
        if(isBlank(requestInputPath)) {
            throw new CommandException("Request input path is null or empty");
        }

        RequestParser<?> requestParser = findOrFail(RequestParser.class, settings.getRequestType());
        RequestInput<?> requestInput = requestParser.parseInput(settings, Paths.get(requestInputPath));

        if(parentRequestInput != null) {
            RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, settings.getRequestType());
            modifier.mergeAPI(settings, parentRequestInput.getApi(), requestInput.getApi());
        }

        int times = settings.getTimes() > 0 ? settings.getTimes() : EXECUTE_ONCE;
        InputNameResolver inputNameResolver = new InputNameResolver(settings,
            requestInputPath, requestInput);
        String inputName = inputNameResolver.filterAndResolve(settings.getInputName(), settings.getFilterName());
        boolean isExecutionAsFlow = requestInput.getFlows().get(inputName) != null;
        ExecutionStats stats = new ExecutionStats(times);
        AtomicReference<String> inputNameCarrier = new AtomicReference<>();

        if(isExecutionAsFlow) {
            trace.validateExecution(requestInputPath, inputName);
        }

        try {
            IntStream.range(0, times)
                .parallel()
                .mapToObj(index -> new ExecutionIndex(index, index + 1, times))
                .forEach(index -> {
                    if(isExecutionAsFlow) {
                        Pair<String, ExecutionStats> flowStats = processFlow(index, trace.nextIteration(),
                            inputName, requestInputPath, requestInput,
                            settings.createForNextExecution(), stepTag);

                        inputNameCarrier.set(flowStats.getLeft());
                        stats.count(flowStats.getRight().computeStatus());
                    } else {
                        Pair<String, ExecutionStats> requestStats = processRequest(index,
                            inputName, requestInputPath, requestInput,
                            settings.createForNextExecution(), stepTag);

                        inputNameCarrier.set(requestStats.getLeft());
                        stats.count(requestStats.getRight().computeStatus());
                    }
                });

            return stats.computeStatus();
        } finally {
            if(times > 1) {
                LOGGER.info("Execution completed:"
                    + "\n  inputFile=" + requestInputPath
                    + "\n  inputName=" + inputNameCarrier.get()
                    + "\n  environment=" + settings.getEnvironment()
                    + (stepTag != null ? "\n  flow=" + stepTag.getFlowLabel() : "")
                    + (stepTag != null ? "\n  step=" + stepTag.getStepLabel() : "")
                    + "\n  executions=" + stats.getExecutions()
                    + "\n  stats=" + stats);
            }
        }
    }

    private Pair<String, ExecutionStats> processFlow(ExecutionIndex index, ExecutionTrace trace,
            String flowName, String requestInputPath, RequestInput<?> requestInput, Settings settings,
            StepTag stepTagParent) {
        FlowEntry flow = requestInput.getFlows().get(flowName);

        if(isEmpty(flow.getSteps())) {
            throw new CommandException("No steps defined for the flow: "
                + flowName + " - " + requestInputPath);
        }

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, settings.getRequestType());
        modifier.expandFlow(settings, flow);

        if(settings.getRequestBehaviour().isPrintBehaviour()) {
            LOGGER.info("Flow-Name: " + flow.getName()
                + (isNotBlank(flow.getDescription()) ? "\nDescription: " + flow.getDescription() : "")
                + "\nRequest-Input-Path: " + requestInputPath);
            LOGGER.info("");
        } else {
            LOGGER.fine("Executing flow:"
                + "\n  index=" + index
                + "\n  flowName=" + flowName
                + "\n  requestInputPath=" + requestInputPath
                + (stepTagParent != null ? "\n  flow=" + stepTagParent.getFlowLabel() : "")
                + (stepTagParent != null ? "\n  step=" + stepTagParent.getStepLabel() : ""));
        }

        ExecutionStats stats = new ExecutionStats(flow.getSteps().size());
        int stepIndex = 1;

        for(StepEntry step : flow.getSteps()) {
            StepTag stepTag = new StepTag(requestInputPath, flowName, stepIndex, flow.getSteps().size());

            if(stepIndex < settings.getStartInStepIndex()) {
                LOGGER.info("Step skipped - "
                    + "[" + stepTag.getFlowLabel() + "]"
                    + " index=" + index
                    + " step="+ stepTag.getStepLabel());
                LOGGER.info("");

                stats.count(ExecutionStatus.SKIPPED);
                stepIndex++;

                continue;
            }

            Settings stepSettings = settings.createForStep();

            try {
                if(settings.getRequestBehaviour().isPrintBehaviour()) {
                    LOGGER.info("Step: "+ stepTag.getStepLabel());
                } else {
                    LOGGER.info("Executing step - "
                        + "[" + stepTag.getFlowLabel() + "]"
                        + " index=" + index
                        + " step="+ stepTag.getStepLabel());
                    LOGGER.info("");
                }

                ExecutionStatus status = executeStep(stepSettings, trace, step, requestInput, requestInputPath, stepTag);
                stats.count(status);
            } catch(RecursiveCommandException ex) {
                throw ex;
            } catch(CommandException | RequestException ex) {
                throw new CommandException(
                    "[" + requestInputPath + "/" + flowName + "]"
                    + " index=" + index
                    + " step="+ stepTag
                    + " - " + ex.getMessage());
            }

            stepIndex++;
        }

        if(!settings.getRequestBehaviour().isPrintBehaviour()) {
            LOGGER.info((stepTagParent != null ? "Step: " : "") + "Flow completed:"
                + "\n  index=" + index
                + "\n  inputFile=" + requestInputPath
                + "\n  flowName=" + flowName
                + "\n  environment=" + settings.getEnvironment()
                + "\n  steps=" + stats.getExecutions()
                + "\n  stats=" + stats);
        }

        return new Pair<>(flowName, stats);
    }

    private ExecutionStatus executeStep(Settings settings, ExecutionTrace trace, StepEntry step,
            RequestInput<?> parentRequestInput, String requestInputPath, StepTag stepTag) {
        executeOptions(settings, step.getOptions());

        if(isNotBlank(step.getRequestInputPath())) {
            return executeInputPath(trace, settings, parentRequestInput, step.getRequestInputPath(), stepTag);
        } else {
            return executeInputPath(trace, settings, parentRequestInput, requestInputPath, stepTag);
        }
    }

    private Pair<String, ExecutionStats> processRequest(ExecutionIndex index,
            String requestName, String requestInputPath, RequestInput<?> requestInput,
            Settings settings, StepTag stepTag) {
        if(isEmpty(requestInput.getRequests())) {
            throw new CommandException("No requests are defined in the request file: " + requestInputPath);
        }

        StringExpander stringExpander = new StringExpander(settings);
        RequestEntry<? extends MockEntry> request = requestInput.getRequests().get(requestName);

        if(request == null) {
            throw new CommandException("No request defined for name: "
                + requestName + " - " + requestInputPath);
        }

        Optional<ExecutionStatus> authStatus = processAuthentication(requestName,
                requestInputPath, requestInput, settings.createForNextExecution(), stepTag);
        ExecutionStats stats;

        if(authStatus.isPresent()) {
            stats = new ExecutionStats(AUTH_AND_REQUEST_EXECUTIONS);
            stats.count(authStatus.get());
        } else {
            stats = new ExecutionStats(ONLY_REQUEST_EXECUTIONS);
        }

        if(settings.getRequestBehaviour().isPrintBehaviour()) {
            LOGGER.info("Request-Name: " + request.getName()
                + (isNotBlank(request.getDescription()) ? "\nDescription: " + request.getDescription() : "")
                + "\nRequest-Input-Path: " + requestInputPath);
            LOGGER.info("");
        } else {
            LOGGER.fine("Executing request:"
                + "\n  index=" + index
                + "\n  requestName=" + requestName
                + "\n  requestInputPath=" + requestInputPath
                + (stepTag != null ? "\n  flow=" + stepTag.getFlowLabel() : "")
                + (stepTag != null ? "\n  step=" + stepTag.getStepLabel() : ""));
        }

        executeOptions(settings, request.getOptions());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, settings.getRequestType());
        modifier.mergeHeader(requestInput.getApi(), request);

        try {
            ExecutionStatus status = executeRequest(index, stringExpander,
                requestInputPath, requestInput.getApi(), request, stepTag);

            stats.count(status);

            return new Pair<>(requestName, stats);
        } catch(CommandException | RequestException ex) {
            throw new CommandException(
                "[" + requestInputPath + "/" + requestName + "] "
                + " index=" + index
                + " - " + ex.getMessage());
        }
    }

    @SuppressWarnings("unchecked")
    private Optional<ExecutionStatus> processAuthentication(String requestName,
            String requestInputPath, RequestInput<?> requestInput, Settings settings,
            StepTag stepTag) {
        lock.lock();

        try {
            RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, settings.getRequestType());
            List<?> authRequests = modifier.getAuthenticationIfExists(requestName,
                requestInput, settings, (s, o) -> executeOptionsNoSkip(s, o));

            if(authRequests.isEmpty()) {
                return Optional.empty();
            }

            ExecutionStats stats = new ExecutionStats(authRequests.size());

            for(Object authRequestRaw : authRequests) {
                RequestEntry<? extends MockEntry> authRequest =
                    (RequestEntry<? extends MockEntry>) authRequestRaw;
                ExecutionIndex index = new ExecutionIndex(0, 1, 1);

                LOGGER.fine("Executing authentication request"
                    + "\n  index=" + index
                    + "\n  authName=" + authRequest.getName()
                    + "\n  requestInputPath=" + requestInputPath
                    + (stepTag != null ? "\n  flow=" + stepTag.getFlowLabel() : "")
                    + (stepTag != null ? "\n  step=" + stepTag.getStepLabel() : ""));

                try {
                    ExecutionStatus status = executeRequest(index, new StringExpander(settings.createForStep()),
                        requestInputPath, null, authRequest,
                        stepTag);

                    stats.count(status);
                } catch(CommandException | RequestException ex) {
                    throw new CommandException(
                        "[" + requestInputPath+ "/" + authRequest.getName() + "] "
                        + " index=" + index
                        + (stepTag != null ? " flow=" + stepTag.getFlowLabel() : "")
                        + (stepTag != null ? " step=" + stepTag.getStepLabel() : "")
                        + " - " + ex.getMessage());
                }
            }

            return Optional.of(stats.computeStatus());
        } finally {
            lock.unlock();
        }
    }

    private ExecutionStatus executeRequest(ExecutionIndex index, StringExpander stringExpander, String requestInputPath,
            RequestEntry<? extends MockEntry> api, RequestEntry<? extends MockEntry> request, StepTag stepTag) {
        Settings settings = stringExpander.getSettings();
        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, settings.getRequestType());
        RequestExecutor<?, ?> executor = findByNameOrFail(RequestExecutor.class, settings.getRequestType());
        ResponseProcessor<?, ?> processor = findByNameOrFail(ResponseProcessor.class, settings.getRequestType());
        Optional<AssertionResult> conditionsResult = executor.accepts(settings, request);

        if(!settings.getRequestBehaviour().isPrintBehaviour()
                && conditionsResult.isPresent()
                && !conditionsResult.get().isPassed()) {
            LOGGER.info(executionCompletedMessage(settings, index, requestInputPath,
                request, null, SKIPPED, null, stepTag));
            conditionsResult.get().getFailedMessages().forEach(LOGGER::info);
            LOGGER.info("");
            return FAILED;
        }

        modifier.mergeAPI(settings, api, request);
        modifier.expandRequest(settings, request);

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
                    request, response, status, historyEntry, stepTag));
                assertionResult.get().getFailedMessages().forEach(LOGGER::warning);
            } else {
                LOGGER.info(executionCompletedMessage(settings, index, requestInputPath,
                    request, response, status, historyEntry, stepTag));
            }
        } else if(!settings.getRequestBehaviour().isPrintBehaviour()) {
            LOGGER.info(executionCompletedMessage(settings, index, requestInputPath,
                request, response, status, historyEntry, stepTag));
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

        exec(settings, false, INFO, settings.getOpenEditorCommand());
    }

    private String executionCompletedMessage(Settings settings, ExecutionIndex index,
            String inputPath, RequestEntry<?> request, ResponseEntry response,
            ExecutionStatus status, HistoryEntry historyEntry, StepTag stepTag) {
        return (stepTag != null ? "Step: " : "") + "Request completed:"
            + (response != null && response.getResult() != null
                ? "\n  result=" + response.getResult() : "")
            + "\n  inputFile=" + inputPath
            + "\n  requestName=" + request.getName()
            + "\n  environment=" + settings.getEnvironment()
            + (stepTag != null ? "\n  flow=" + stepTag.getFlowLabel() : "")
            + (stepTag != null ? "\n  step=" + stepTag.getStepLabel() : "")
            + "\n  index=" + index
            + "\n  executionStatus=" + status
            + "\n  executionTag=" + settings.getExecutionTag()
            + (historyEntry != null 
                ? "\n  executionTime(nano)=" + historyEntry.getNanoTime() : "");
    }
}
