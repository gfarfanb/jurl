package com.legadi.jurl.executor;

import java.util.List;

import com.legadi.jurl.exception.SkipExecutionException;
import com.legadi.jurl.options.OptionsProcessor;
import com.legadi.jurl.options.OptionsProcessor.OptionEntry;

import static com.legadi.jurl.common.Settings.clearSettings;

public class RequestProcessor {

    private final OptionsProcessor optionsProcessor;

    public RequestProcessor(String[] args) {
        this.optionsProcessor = new OptionsProcessor(args);

        optionsProcessor.registerAddOnOptions();
    }

    public void execute() {
        executeOptions();
        executeRequest();
        processResults();
        clearSettings();
    }

    private void executeOptions() {
        List<OptionEntry> optionEntries = optionsProcessor.getOptionEntries();
        
        for(OptionEntry optionEntry : optionEntries) {
            if(!optionEntry.getLeft().execute(optionEntry.getRight())) {
                throw new SkipExecutionException();
            }
        }
    }

    private void executeRequest() {

    }

    private void processResults() {
        
    }
}
