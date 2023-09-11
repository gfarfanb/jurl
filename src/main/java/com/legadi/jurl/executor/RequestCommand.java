package com.legadi.jurl.executor;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.exception.SkipExecutionException;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.RequestInputRaw;
import com.legadi.jurl.model.ResponseEntry;
import com.legadi.jurl.model.StepEntry;
import com.legadi.jurl.options.OptionsReader;
import com.legadi.jurl.options.OptionsReader.OptionEntry;
import com.legadi.jurl.options.SetInputNameOption;

import static com.legadi.jurl.common.LoaderUtils.jsonToObject;
import static com.legadi.jurl.common.LoaderUtils.loadCredentials;
import static com.legadi.jurl.common.LoaderUtils.loadJsonFile;
import static com.legadi.jurl.common.LoaderUtils.loadJsonProperties;
import static com.legadi.jurl.common.StringUtils.isBlank;
import static com.legadi.jurl.common.StringUtils.isNotBlank;
import static com.legadi.jurl.common.StringUtils.replaceAllInContent;
import static com.legadi.jurl.executor.RequestHandlersRegistry.findByRequest;

public class RequestCommand {

    private final OptionsReader optionsReader;
    private final Set<String> registeredInputPaths;

    public RequestCommand(String[] args) {
        this.optionsReader = new OptionsReader(args);
        this.registeredInputPaths = new HashSet<>();

        optionsReader.registerAddOnOptions();
    }

    public void execute() {
        Settings settings = new Settings();

        executeOptions(settings, optionsReader.getOptionEntries());
        executeInput(settings, optionsReader.getRequestInputPath(), true);
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

    private void executeInput(Settings settings, String requestInputPath, boolean isMainInput) {
        if(isBlank(requestInputPath)) {
            throw new CommandException("Request input path is null or empty");
        }
        if(registeredInputPaths.contains(requestInputPath)) {
            throw new CommandException("Request input was already processed: " + requestInputPath);
        }

        registeredInputPaths.add(requestInputPath);

        RequestInputRaw requestInput = loadJsonFile(requestInputPath,
            new TypeToken<RequestInputRaw>() {}, false);

        requestInput.setPath(requestInputPath);

        executeInput(settings, requestInput, isMainInput);
    }

    private void executeInput(Settings settings, RequestInputRaw requestInput, boolean isMainInput) {
        loadConfig(requestInput, settings, isMainInput);

        int times = settings.getTimes() > 0 ? settings.getTimes() : 1;

        IntStream.range(0, times)
            .parallel()
            .forEach(index -> {
                if(settings.isExecutionAsFlow()) {
                    processFlow(index, requestInput, settings.createForNextExecution());
                } else {
                    processRequest(index, requestInput, settings.createForNextExecution());
                }
            });
    }

    private void loadConfig(RequestInputRaw requestInput, Settings settings, boolean isMainInput) {
        String environment = settings.getEnvironment();
        String configFile = settings.getConfigFileName();
        String credentialsFile = settings.getCredentialsFileName();

        Settings.mergeProperties(environment, loadJsonProperties(configFile, false));
        Settings.mergeCredentials(environment, loadCredentials(credentialsFile, false));

        if(requestInput.getConfigs() != null) {
            Map<String, String> fileConfig = requestInput.getConfigs().getOrDefault(environment, new HashMap<>());

            if(isMainInput) {
                Settings.mergeProperties(environment, fileConfig);
            } else {
                settings.mergeOverrideProperties(fileConfig);
            }
        }
    }

    private void processFlow(int index, RequestInputRaw requestInput, Settings settings) {
        if(requestInput.getFlows() == null || requestInput.getFlows().isEmpty()) {
            throw new CommandException("No flows are defined in the request file: " + requestInput.getPath());
        }

        Pair<String, String[]> flowDef = pickFlow(requestInput, settings);

        if(flowDef.getRight() == null || flowDef.getRight().length < 1) {
            throw new CommandException("No steps defined for the flow: "
                + flowDef.getLeft() + " - " + requestInput.getPath());
        }

        List<StepEntry> steps = Arrays.stream(flowDef.getRight())
            .parallel()
            .map(step -> replaceAllInContent(settings, step))
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
            executeInput(settings, requestInput, false);
        } else {
            executeInput(settings, step.getRequestInputPath(), false);
        }
    }

    private void processRequest(int index, RequestInputRaw requestInput, Settings settings) {
        if(requestInput.getRequests() == null || requestInput.getRequests().isEmpty()) {
            throw new CommandException("No requests are defined in the request file: " + requestInput.getPath());
        }

        Pair<String, String> requestDef = pickRequest(requestInput, settings);
        if(isBlank(requestDef.getRight())) {
            throw new CommandException("No request defined for name: "
                + requestDef.getLeft() + " - " + requestInput.getPath());
        }

        String requestRaw = replaceAllInContent(settings, requestDef.getRight());
        RequestEntry requestEntry = jsonToObject(requestRaw, new TypeToken<RequestEntry>() {});

        requestEntry.setName(requestDef.getLeft());

        try {
            executeRequest(settings, requestEntry, requestRaw);
        } catch(CommandException | RequestException ex) {
            throw new CommandException(
                "[" + requestInput.getPath() + "/" + requestEntry.getName() + "] "
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

    private void executeRequest(Settings settings, RequestEntry requestEntry, String requestRaw) {
        Pair<RequestExecutor<?, ?>, ResponseProcessor<?, ?>> handlers = findByRequest(requestEntry);
        RequestExecutor<?, ?> executor = handlers.getLeft();
        ResponseProcessor<?, ?> processor = handlers.getRight();
        RequestEntry request = jsonToObject(requestRaw, executor.type());

        long beginTime = System.nanoTime();
        ResponseEntry response = executor.execute(settings, request);
        long endTime = System.nanoTime();

        processor.process(settings, request, response, endTime - beginTime);
    }
}
