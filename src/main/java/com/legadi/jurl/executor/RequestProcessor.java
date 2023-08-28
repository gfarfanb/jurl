package com.legadi.jurl.executor;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.common.SettingsSetter;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.exception.SkipExecutionException;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.RequestInputRaw;
import com.legadi.jurl.model.ResponseEntry;
import com.legadi.jurl.model.StepEntry;
import com.legadi.jurl.options.OptionsProcessor;
import com.legadi.jurl.options.OptionsProcessor.OptionEntry;

import static com.legadi.jurl.common.Loader.jsonToObject;
import static com.legadi.jurl.common.Loader.loadCredentials;
import static com.legadi.jurl.common.Loader.loadJsonFile;
import static com.legadi.jurl.common.Loader.loadJsonProperties;
import static com.legadi.jurl.common.StringUtils.isBlank;
import static com.legadi.jurl.common.StringUtils.isNotBlank;
import static com.legadi.jurl.executor.RequestExecutorRegistry.getExecutor;

public class RequestProcessor {

    private final OptionsProcessor optionsProcessor;
    private final Set<String> registeredInputPaths;

    public RequestProcessor(String[] args) {
        this.optionsProcessor = new OptionsProcessor(args);
        this.registeredInputPaths = new HashSet<>();

        optionsProcessor.registerAddOnOptions();
    }

    public void execute() {
        SettingsSetter settingsSetter = new SettingsSetter();

        executeOptions(settingsSetter, optionsProcessor.getOptionEntries());
        executeInput(settingsSetter, optionsProcessor.getRequestInputPath());
    }

    private void executeOptions(SettingsSetter settingsSetter, List<OptionEntry> optionEntries) {
        for(OptionEntry optionEntry : optionEntries) {
            boolean canContinue = optionEntry.getLeft().execute(
                settingsSetter, optionEntry.getRight()
            );

            if(!canContinue) {
                throw new SkipExecutionException();
            }
        }
    }

    private void executeInput(SettingsSetter settingsSetter, String requestInputPath) {
        if(isBlank(requestInputPath)) {
            throw new CommandException("Request input path is null or empty");
        }
        if(registeredInputPaths.contains(requestInputPath)) {
            throw new CommandException("Request input was already executed: " + requestInputPath);
        }
        
        registeredInputPaths.add(requestInputPath);

        RequestInputRaw requestInput = loadJsonFile(requestInputPath,
            new TypeToken<RequestInputRaw>() {}, false);

        requestInput.setPath(requestInputPath);

        loadConfig(requestInput, settingsSetter);

        if(settingsSetter.getSettings().isExecutionAsFlow()) {
            processFlow(requestInput, settingsSetter);
        } else {
            processRequest(requestInput, settingsSetter);
        }
    }

    private void loadConfig(RequestInputRaw requestInput, SettingsSetter settingsSetter) {
        String env = settingsSetter.getSettings().getEnvironment();
        String fileConfig = "./config." + env + ".json";

        settingsSetter.mergeProperties(fileConfig, loadJsonProperties(fileConfig, false));
        settingsSetter.mergeCredentials(loadCredentials("./credentials." + env + ".json", false));

        if(requestInput.getConfigs() != null) {
            settingsSetter.mergeProperties(requestInput.getPath(), 
                requestInput.getConfigs().getOrDefault(env, new HashMap<>()));
        }

        settingsSetter.loadPriorityConfig();
    }

    private void processFlow(RequestInputRaw requestInput, SettingsSetter settingsSetter) {
        if(requestInput.getFlows() == null || requestInput.getFlows().isEmpty()) {
            throw new CommandException("No flows are defined in the request file: " + requestInput.getPath());
        }

        Pair<String, String[]> flowDef = pickFlow(requestInput, settingsSetter);

        if(flowDef.getRight() == null || flowDef.getRight().length < 1) {
            throw new CommandException("No steps defined for the flow: "
                + flowDef.getLeft() + " - " + requestInput.getPath());
        }

        AtomicReference<Settings> stepSettings = new AtomicReference<>(settingsSetter.getSettings());
        List<StepEntry> steps = Arrays.stream(flowDef.getRight())
            .parallel()
            .map(step -> stepSettings.get().replaceAllInContent(step))
            .map(step -> jsonToObject(step, new TypeToken<StepEntry>() {}))
            .collect(Collectors.toList());
        int stepIndex = 1;

        for(StepEntry step : steps) {
            step.setFlowName(flowDef.getLeft());
            stepSettings.set(stepSettings.get().createForNextExecution());

            try {
                executeStep(stepSettings.get(), step);
            } catch(CommandException | RequestException ex) {
                throw new CommandException(
                    "[" + requestInput.getPath() + "/" + step.getFlowName() + "]"
                    + " step("+ stepIndex + "/" + steps.size() + ") "
                    + ex.getMessage());
            }

            stepIndex++;
        }
    }

    private Pair<String, String[]> pickFlow(RequestInputRaw requestInput, SettingsSetter settingsSetter) {
        String name = isNotBlank(settingsSetter.getSettings().getInputName())
            ? settingsSetter.getSettings().getInputName()
            : requestInput.getDefaultFlow();

        if(isNotBlank(name)) {
            return new Pair<>(name, requestInput.getFlows().get(name));
        } else {
            String flowName = requestInput.getFlows().keySet().stream().findFirst().get();
            return new Pair<>(flowName, requestInput.getFlows().get(flowName));
        }
    }

    private void executeStep(Settings settings, StepEntry step) {
        SettingsSetter settingsSetter = new SettingsSetter(settings);

        if(isBlank(step.getRequestInputPath())) {
            throw new CommandException("Request input path is null or empty");
        }

        executeOptions(settingsSetter, optionsProcessor.mapToOptionEntries(step.getOptions()));
        executeInput(settingsSetter, step.getRequestInputPath());
    }

    private void processRequest(RequestInputRaw requestInput, SettingsSetter settingsSetter) {
        if(requestInput.getRequests() == null || requestInput.getRequests().isEmpty()) {
            throw new CommandException("No requests are defined in the request file: " + requestInput.getPath());
        }

        Pair<String, String> requestDef = pickRequest(requestInput, settingsSetter);
        if(isBlank(requestDef.getRight())) {
            throw new CommandException("No request defined for name: "
                + requestDef.getLeft() + " - " + requestInput.getPath());
        }

        String requestRaw = settingsSetter.getSettings().replaceAllInContent(requestDef.getRight());
        RequestEntry requestEntry = jsonToObject(requestRaw, new TypeToken<RequestEntry>() {});

        requestEntry.setName(requestDef.getLeft());

        try {
            executeRequest(settingsSetter.getSettings().createForNextExecution(),
                requestEntry, requestRaw);
        } catch(CommandException | RequestException ex) {
            throw new CommandException(
                "[" + requestInput.getPath() + "/" + requestEntry.getName() + "] "
                + ex.getMessage());
        }
    }

    private Pair<String, String> pickRequest(RequestInputRaw requestInput, SettingsSetter settingsSetter) {
        String name = isNotBlank(settingsSetter.getSettings().getInputName())
            ? settingsSetter.getSettings().getInputName()
            : requestInput.getDefaultRequest();

        if(isNotBlank(name)) {
            return new Pair<>(name, requestInput.getRequests().get(name));
        } else {
            String requestName = requestInput.getRequests().keySet().stream().findFirst().get();
            return new Pair<>(requestName, requestInput.getRequests().get(requestName));
        }
    }

    private void executeRequest(Settings settings, RequestEntry requestEntry, String requestRaw) {
        RequestExecutor<?, ?> executor = getExecutor(requestEntry);
        RequestEntry request = jsonToObject(requestRaw, executor.type());
        ResponseEntry response = executor.execute(settings, request);

        processResults(response);
    }

    private void processResults(ResponseEntry response) {

    }
}
