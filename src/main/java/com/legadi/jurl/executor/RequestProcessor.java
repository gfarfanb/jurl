package com.legadi.jurl.executor;

import java.util.List;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.SkipExecutionException;
import com.legadi.jurl.model.RequestDefinition;
import com.legadi.jurl.model.RequestInputRaw;
import com.legadi.jurl.model.StepDefinition;
import com.legadi.jurl.options.OptionsProcessor;
import com.legadi.jurl.options.OptionsProcessor.OptionEntry;

import static com.legadi.jurl.common.Loader.jsonToObject;
import static com.legadi.jurl.common.Loader.loadJsonFile;
import static com.legadi.jurl.common.Settings.mergeProperties;
import static com.legadi.jurl.common.StringUtils.isNotBlank;

public class RequestProcessor {

    private final OptionsProcessor optionsProcessor;

    public RequestProcessor(String[] args) {
        this.optionsProcessor = new OptionsProcessor(args);

        optionsProcessor.registerAddOnOptions();
    }

    public void execute() {
        executeOptions();
        executeInput();
    }

    private void executeOptions() {
        List<OptionEntry> optionEntries = optionsProcessor.getOptionEntries();
        
        for(OptionEntry optionEntry : optionEntries) {
            if(!optionEntry.getLeft().execute(optionEntry.getRight())) {
                throw new SkipExecutionException();
            }
        }
    }

    private void executeInput() {
        RequestInputRaw requestInput = loadJsonFile(optionsProcessor.getRequestInputPath(), RequestInputRaw.class);

        if(requestInput.getConfig() != null) {
            mergeProperties(requestInput.getConfig());
        }

        Settings settings = new Settings();

        if(isNotBlank(requestInput.getRequest())) {
            RequestDefinition request = jsonToObject(
                settings.replaceAllInContent(requestInput.getRequest()),
                RequestDefinition.class
            );

            executeRequest(request);
        }

        if(requestInput.getSteps() != null) {
            requestInput.getSteps()
                .stream()
                .map(step -> settings.replaceAllInContent(step))
                .map(step -> jsonToObject(step, StepDefinition.class))
                .forEach(this::executeStep);
        }
    }

    private void executeRequest(RequestDefinition request) {
        processResults();
    }

    private void executeStep(StepDefinition step) {
        processResults();
    }

    private void processResults() {
        
    }
}
