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
import com.legadi.jurl.model.RequestDefinition;
import com.legadi.jurl.model.RequestInputRaw;
import com.legadi.jurl.model.StepDefinition;
import com.legadi.jurl.options.OptionsProcessor;
import com.legadi.jurl.options.OptionsProcessor.OptionEntry;

import static com.legadi.jurl.common.Loader.jsonToObject;
import static com.legadi.jurl.common.Loader.loadJsonFile;
import static com.legadi.jurl.common.StringUtils.isBlank;
import static com.legadi.jurl.common.StringUtils.isNotBlank;

public class RequestProcessor {

    private final OptionsProcessor optionsProcessor;
    private final Set<String> registeredInputPaths;

    public RequestProcessor(String[] args) {
        this.optionsProcessor = new OptionsProcessor(args);
        this.registeredInputPaths = new HashSet<>();

        optionsProcessor.registerAddOnOptions();
    }

    public void execute() {
        executeOptions(new SettingsSetter(), optionsProcessor.getOptionEntries());
        executeInput(new SettingsSetter(), optionsProcessor.getRequestInputPath());
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

        RequestInputRaw requestInput = loadJsonFile(requestInputPath, new TypeToken<RequestInputRaw>() {}, false);

        if(requestInput.getConfigs() != null) {
            settingsSetter.mergeProperties(requestInput.getConfigs().getOrDefault(
                settingsSetter.getSettings().getEnvironment(), new HashMap<>()
            ));
        }

        settingsSetter.loadPriorityConfig();

        if(settingsSetter.getSettings().isExecutionAsFlow()) {
            processFlow(requestInput, settingsSetter, requestInputPath);
        } else {
            processRequest(requestInput, settingsSetter, requestInputPath);
        }
    }

    private void processFlow(RequestInputRaw requestInput, SettingsSetter settingsSetter, String requestInputPath) {
        if(requestInput.getFlows() == null || requestInput.getFlows().isEmpty()) {
            throw new CommandException("No flows are defined in the request file: " + requestInputPath);
        }

        Pair<String, String[]> flowDef = pickFlow(requestInput);

        if(flowDef.getRight() == null || flowDef.getRight().length < 1) {
            throw new CommandException("No steps defined for the flow: "
                + flowDef.getLeft() + " - " + requestInputPath);
        }

        AtomicReference<Settings> stepSettings = new AtomicReference<>(settingsSetter.getSettings());
        List<StepDefinition> steps = Arrays.stream(flowDef.getRight())
            .parallel()
            .map(step -> stepSettings.get().replaceAllInContent(step))
            .map(step -> jsonToObject(step, StepDefinition.class))
            .collect(Collectors.toList());
        int stepIndex = 1;

        for(StepDefinition step : steps) {
            stepSettings.set(stepSettings.get().createForNextExecution());

            try {
                executeStep(flowDef.getLeft(), stepSettings.get(), step);
            } catch(CommandException | RequestException ex) {
                throw new CommandException("[" + requestInputPath
                    + "] step(" + stepIndex + "/" + steps.size() + ") " + ex.getMessage());
            }

            stepIndex++;
        }
    }

    private Pair<String, String[]> pickFlow(RequestInputRaw requestInput) {
        if(isNotBlank(requestInput.getDefaultFlow())) {
            return new Pair<>(
                requestInput.getDefaultFlow(),
                requestInput.getFlows().get(requestInput.getDefaultFlow())
            );
        } else {
            String flowName = requestInput.getFlows().keySet().stream().findFirst().get();
            return new Pair<>(flowName, requestInput.getFlows().get(flowName));
        }
    }

    private void executeStep(String flowName, Settings settings, StepDefinition step) {
        SettingsSetter settingsSetter = new SettingsSetter(settings);

        if(isBlank(step.getRequestInputPath())) {
            throw new CommandException("Request input path is null or empty");
        }

        executeOptions(settingsSetter, optionsProcessor.mapToOptionEntries(step.getOptions()));
        executeInput(settingsSetter, step.getRequestInputPath());
    }

    private void processRequest(RequestInputRaw requestInput, SettingsSetter settingsSetter, String requestInputPath) {
        if(requestInput.getRequests() == null || requestInput.getRequests().isEmpty()) {
            throw new CommandException("No requests are defined in the request file: " + requestInputPath);
        }

        Pair<String, String> requestDef = pickRequest(requestInput);
        if(isBlank(requestDef.getRight())) {
            throw new CommandException("No request defined for name: "
                + requestDef.getLeft() + " - " + requestInputPath);
        }

        RequestDefinition request = jsonToObject(
            settingsSetter.getSettings().replaceAllInContent(requestDef.getRight()),
            RequestDefinition.class
        );

        try {
            executeRequest(requestDef.getLeft(), settingsSetter.getSettings().createForNextExecution(), request);
        } catch(CommandException | RequestException ex) {
            throw new CommandException("[" + requestInputPath + "] " + ex.getMessage());
        }
    }

    private Pair<String, String> pickRequest(RequestInputRaw requestInput) {
        if(isNotBlank(requestInput.getDefaultRequest())) {
            return new Pair<>(
                requestInput.getDefaultRequest(),
                requestInput.getRequests().get(requestInput.getDefaultRequest())
            );
        } else {
            String requestName = requestInput.getRequests().keySet().stream().findFirst().get();
            return new Pair<>(requestName, requestInput.getRequests().get(requestName));
        }
    }

    private void executeRequest(String requestName, Settings settings, RequestDefinition request) {
        processResults();
    }

    private void processResults() {
        
    }
}
