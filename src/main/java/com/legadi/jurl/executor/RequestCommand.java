package com.legadi.jurl.executor;

import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isEmpty;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.common.JsonUtils.jsonToObject;
import static com.legadi.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.jurl.common.JsonUtils.toJsonString;
import static com.legadi.jurl.common.LoaderUtils.loadCredentials;
import static com.legadi.jurl.common.RequestUtils.mergeRequestHeader;
import static com.legadi.jurl.common.WriterUtils.appendToFile;
import static com.legadi.jurl.common.WriterUtils.writeFile;
import static com.legadi.jurl.executor.RequestHandlersRegistry.findByRequest;

import java.io.File;
import java.nio.file.Path;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.ExecutionLevels;
import com.legadi.jurl.common.OutputPathBuilder;
import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.common.StringExpander;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.InvalidAssertionsFoundException;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.exception.SkipExecutionException;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.HistoryEntry;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.RequestInputRaw;
import com.legadi.jurl.model.ResponseEntry;
import com.legadi.jurl.model.StepEntry;
import com.legadi.jurl.options.OptionsReader;
import com.legadi.jurl.options.OptionsReader.OptionEntry;
import com.legadi.jurl.options.SetInputNameOption;

public class RequestCommand {

    private static final int FIRST_EXECUTION = 1;

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

        RequestInputRaw requestInput = loadJsonFile(requestInputPath, new TypeToken<RequestInputRaw>() {});

        requestInput.setPath(requestInputPath);

        executeInputRaw(settings, requestInput, isMainInput);
    }

    private void executeInputRaw(Settings settings, RequestInputRaw requestInput, boolean isMainInput) {
        loadConfig(requestInput, settings, isMainInput);

        int times = settings.getTimes() > 0 ? settings.getTimes() : FIRST_EXECUTION;

        IntStream.range(0, times)
            .parallel()
            .forEach(index -> {
                if(settings.isExecutionAsFlow()) {
                    if(index == FIRST_EXECUTION) {
                        executionLevels.nextLevel();
                    }
                    processFlow(index, requestInput, new StringExpander(settings.createForNextExecution()));
                } else {
                    processRequest(index, requestInput, new StringExpander(settings.createForNextExecution()));
                }
            });
    }

    private void loadConfig(RequestInputRaw requestInput, Settings settings, boolean isMainInput) {
        String environment = settings.getEnvironment();
        Path configPath = settings.getConfigFilePath();
        Path credentialsPath = settings.getCredentialsFilePath();

        Settings.mergeProperties(environment, loadJsonProperties(configPath));
        Settings.mergeCredentials(environment, loadCredentials(credentialsPath));

        if(isNotEmpty(requestInput.getConfigs())) {
            Map<String, String> fileConfig = requestInput.getConfigs().getOrDefault(environment, new HashMap<>());

            if(isMainInput) {
                Settings.mergeProperties(environment, fileConfig);
            } else {
                settings.mergeOverrideProperties(fileConfig);
            }
        }
    }

    private void processFlow(int index, RequestInputRaw requestInput, StringExpander stringExpander) {
        if(isEmpty(requestInput.getFlows())) {
            throw new CommandException("No flows are defined in the request file: " + requestInput.getPath());
        }

        Settings settings = stringExpander.getSettings();
        Pair<String, String[]> flowDef = pickFlow(requestInput, settings);

        if(isEmpty(flowDef.getRight())) {
            throw new CommandException("No steps defined for the flow: "
                + flowDef.getLeft() + " - " + requestInput.getPath());
        }

        List<StepEntry> steps = Arrays.stream(flowDef.getRight())
            .parallel()
            .map(stringExpander::replaceAllInContent)
            .map(step -> jsonToObject(step, new TypeToken<StepEntry>() {}))
            .collect(Collectors.toList());
        int stepIndex = 1;

        for(StepEntry step : steps) {
            step.setName(flowDef.getLeft());

            try {
                executeStep(settings.createForNextExecution(), step, requestInput);
            } catch(CommandException | RequestException ex) {
                throw new CommandException(
                    "[" + requestInput.getPath() + "/" + step.getName() + "]"
                    + (settings.getTimes() > 1 ? " index=" + index + " ": "")
                    + " step("+ stepIndex + "/" + steps.size() + ") "
                    + ex.getMessage());
            }

            stepIndex++;
        }
    }

    private Pair<String, String[]> pickFlow(RequestInputRaw requestInput, Settings settings) {
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

    private void executeStep(Settings settings, StepEntry step, RequestInputRaw requestInput) {
        if(isBlank(step.getRequestInputPath())) {
            throw new CommandException("Request input path is null or empty");
        }

        executeOptions(settings, optionsReader.mapToOptionEntries(step.getOptions()));

        if(isNotBlank(step.getRequestInputName())) {
            SetInputNameOption.setInputName(settings, step.getRequestInputName());
            executeInputRaw(settings, requestInput, false);
        } else {
            executeInputPath(settings, step.getRequestInputPath(), false);
        }
    }

    private void processRequest(int index, RequestInputRaw requestInput, StringExpander stringExpander) {
        if(isEmpty(requestInput.getRequests())) {
            throw new CommandException("No requests are defined in the request file: " + requestInput.getPath());
        }

        Settings settings = stringExpander.getSettings();
        Pair<String, String> requestDef = pickRequest(requestInput, settings);

        if(isBlank(requestDef.getRight())) {
            throw new CommandException("No request defined for name: "
                + requestDef.getLeft() + " - " + requestInput.getPath());
        }
        if(index == FIRST_EXECUTION && executionLevels.wasExecuted(requestInput.getPath(), requestDef.getLeft())) {
            throw new CommandException("Request input was already processed: "
                + requestInput.getPath() + "/" + requestDef.getLeft() + " - " + executionLevels.getTrace());
        }

        String apiRaw = null;
        RequestEntry<? extends MockEntry> apiHeader = null;
        
        if(isNotBlank(requestInput.getApi())) {
            apiRaw = stringExpander.replaceAllInContent(requestInput.getApi());
            apiHeader = jsonToObject(apiRaw, new TypeToken<RequestEntry<? extends MockEntry>>() {});
        }
        
        String requestRaw = stringExpander.replaceAllInContent(requestDef.getRight());
        RequestEntry<? extends MockEntry> requestHeader = jsonToObject(requestRaw, new TypeToken<RequestEntry<? extends MockEntry>>() {});

        if(apiHeader != null) {
            mergeRequestHeader(apiHeader, requestHeader);
        }

        try {
            executeRequest(settings, requestHeader, requestInput.getPath(), requestDef.getLeft(),
                apiRaw, requestRaw);
        } catch(CommandException | RequestException ex) {
            throw new CommandException(
                "[" + requestInput.getPath() + "/" + requestDef.getLeft() + "] "
                + (settings.getTimes() > 1 ? " index=" + index + " ": "")
                + ex.getMessage());
        }
    }

    private Pair<String, String> pickRequest(RequestInputRaw requestInput, Settings settings) {
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

    private void executeRequest(Settings settings, RequestEntry<? extends MockEntry> header,
            String requestPath, String name, String apiRaw, String requestRaw) {
        Pair<RequestExecutor<?, ?>, ResponseProcessor<?, ?>> handlers = findByRequest(header);
        RequestExecutor<?, ?> executor = handlers.getLeft();
        ResponseProcessor<?, ?> processor = handlers.getRight();
        RequestEntry<? extends MockEntry> request = jsonToObject(requestRaw, executor.type());

        request.setRequestPath(requestPath);
        request.setName(name);

        if(apiRaw != null) {
            RequestEntry<? extends MockEntry> api = jsonToObject(apiRaw, executor.type());
            executor.mergeAPI(settings, api, request);
        }

        if(isNotBlank(settings.getMergeBodyUsingType())) {
            executor.mergeBody(settings, request);
        }

        if(isNotBlank(settings.getOverrideRequestFilePath())) {
            executor.overrideRequest(settings, request, settings.getOverrideRequestFilePath());
        }

        long beginTime = System.nanoTime();
        ResponseEntry response = executor.execute(settings, request);
        long endTime = System.nanoTime();

        HistoryEntry historyEntry = new HistoryEntry();
        historyEntry.setCurl(response.getCurlCommand());
        historyEntry.setResult(response.getResult());
        historyEntry.setRequestPath(requestPath);
        historyEntry.setRequestName(name);
        historyEntry.setEnvironment(settings.getEnvironment());
        historyEntry.setTimestamp(settings.getTimestamp().toEpochSecond(OffsetDateTime.now().getOffset()));
        historyEntry.setExecutionTag(settings.getExecutionTag());
        historyEntry.setNanoTime(endTime - beginTime);

        Optional<AssertionResult> result = processor.process(settings, request, response);

        result.ifPresent(r -> {
            historyEntry.setAssertions(r.getAssertions());
            historyEntry.setFailures(r.getFailures());
        });

        saveHistory(settings, historyEntry, request.getRequestPath(), request.getName());

        if(result.isPresent() && result.get().isSkip()) {
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
