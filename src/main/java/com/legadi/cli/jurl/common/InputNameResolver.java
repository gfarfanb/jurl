package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;

import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.exception.ConsoleInputException;
import com.legadi.cli.jurl.model.RequestInput;

public class InputNameResolver {

    private static final Logger LOGGER = Logger.getLogger(InputNameResolver.class.getName());

    private final Settings settings;
    private final String requestInputPath;
    private final RequestInput<?> requestInput;

    public InputNameResolver(Settings settings, String requestInputPath,
            RequestInput<?> requestInput) {
        this.settings = settings;
        this.requestInputPath = requestInputPath;
        this.requestInput = requestInput;
    }

    public String resolve(String inputName) {
        if(isNotBlank(inputName)) {
            return inputName;
        }

        List<String> requestNames = listRequestNames();
        ConsoleInput consoleInput = new ConsoleInput(settings, this::appendType);

        try {
            Optional<String> requestName = consoleInput.selectOption(requestNames, requestInput.getDefaultRequest());

            LOGGER.info("Selected: [" + getType(requestName.orElse(null))  +  "] "
                + requestName.orElse(null) + " - " + requestInputPath);
            LOGGER.info("");

            return requestName.orElse(null);
        } catch(ConsoleInputException ex) {
            throw new CommandException(ex.getMessage() + " - " + requestInputPath);
        }
    }

    private List<String> listRequestNames() {
        List<String> requestNames = new ArrayList<>();
        requestNames.addAll(requestInput.getRequests().keySet());
        requestNames.addAll(requestInput.getFlows().keySet());
        requestNames.sort(Comparator.naturalOrder());
        return requestNames;
    }

    private String appendType(String requestName) {
        return getType(requestName) + ":" + requestName;
    }

    private String getType(String requestName) {
        if(requestName == null) {
            return null;
        }
        return requestInput.getFlows().get(requestName) != null
            ? "flow" : "request";
    }
}
