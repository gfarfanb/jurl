package com.legadi.jurl.executor;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

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
        executeInput(new Settings(), new SettingsSetter(), optionsProcessor.getRequestInputPath());
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

    private void executeInput(Settings settings, SettingsSetter settingsSetter, String requestInputPath) {
        if(isBlank(requestInputPath)) {
            throw new CommandException("Request input path is null or empty");
        }
        if(registeredInputPaths.contains(requestInputPath)) {
            throw new CommandException("Request input was already executed: " + requestInputPath);
        }
        
        registeredInputPaths.add(requestInputPath);

        RequestInputRaw requestInput = loadJsonFile(requestInputPath, RequestInputRaw.class);

        if(requestInput.getConfig() != null) {
            settingsSetter.mergeProperties(requestInput.getConfig());
        }

        if(settings.isExecutionAsFlow()) {
            if(requestInput.getSteps() == null || requestInput.getSteps().isEmpty()) {
                throw new CommandException("Flow not defined in the request file: " + requestInputPath);
            }

            AtomicReference<Settings> stepSettings = new AtomicReference<>(settings);
            List<StepDefinition> steps = requestInput.getSteps()
                .parallelStream()
                .map(step -> stepSettings.get().replaceAllInContent(step))
                .map(step -> jsonToObject(step, StepDefinition.class))
                .collect(Collectors.toList());
            int stepIndex = 1;

            for(StepDefinition step : steps) {
                stepSettings.set(stepSettings.get().createForNextExecution());

                try {
                    executeStep(stepSettings.get(), step);
                } catch(CommandException | RequestException ex) {
                    throw new CommandException("[" + requestInputPath
                        + "] step(" + stepIndex + "/" + steps.size() + ") " + ex.getMessage());
                }

                stepIndex++;
            }

            return;
        }

        if(isNotBlank(requestInput.getRequest())) {
            RequestDefinition request = jsonToObject(
                settings.replaceAllInContent(requestInput.getRequest()),
                RequestDefinition.class
            );

            try {
                executeRequest(settings.createForNextExecution(), request);
            } catch(CommandException | RequestException ex) {
                throw new CommandException("[" + requestInputPath + "] " + ex.getMessage());
            }
        } else {
            throw new CommandException("Request not defined in the file: " + requestInputPath);
        }
    }

    private void executeRequest(Settings settings, RequestDefinition request) {
        processResults();
    }

    private void executeStep(Settings settings, StepDefinition step) {
        SettingsSetter settingsSetter = new SettingsSetter(settings);

        if(isBlank(step.getRequestInputPath())) {
            throw new CommandException("Request input path is null or empty");
        }

        executeOptions(settingsSetter, optionsProcessor.mapToOptionEntries(step.getOptions()));
        executeInput(settings, settingsSetter, step.getRequestInputPath());
    }

    private void processResults() {
        
    }
}
