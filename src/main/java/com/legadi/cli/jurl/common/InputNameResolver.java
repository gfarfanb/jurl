package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.FLOW_NAME;
import static com.legadi.cli.jurl.common.CommonUtils.FLOW_TAG;
import static com.legadi.cli.jurl.common.CommonUtils.REQUEST_NAME;
import static com.legadi.cli.jurl.common.CommonUtils.REQUEST_TAG;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;
import java.util.stream.Collectors;

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

        return resolve(listRequestNames());
    }

    public String filterAndResolve(String inputName, String filterName) {
        if(isBlank(inputName) && isNotBlank(filterName)) {
            List<String> requestNames = listRequestNames()
                .stream()
                .filter(name -> name.toLowerCase().contains(filterName.toLowerCase()))
                .collect(Collectors.toList());

            if(requestNames.isEmpty() || requestNames.size() == 1) {
                return filterName;
            }

            return resolve(requestNames);
        } else {
            return resolve(inputName);
        }
    }

    private String resolve(List<String> requestNames) {
        ConsoleInput consoleInput = new ConsoleInput(settings, this::appendType);

        try {
            Optional<String> requestName = consoleInput.selectOption(requestNames, requestInput.getDefaultRequest());

            LOGGER.info("Selected: [" + getType(requestName.orElse(null))  +  "] "
                + requestInputPath + "/" + requestName.orElse(null));
            LOGGER.info("");

            return requestName.orElse(null);
        } catch(ConsoleInputException ex) {
            throw new CommandException(ex.getMessage() + " - " + requestInputPath);
        }
    }

    public List<String> listRequestNames() {
        List<String> requestNames = new ArrayList<>();
        requestNames.addAll(requestInput.getRequests().keySet());
        requestNames.addAll(requestInput.getFlows().keySet());
        requestNames.sort(Comparator.naturalOrder());
        return requestNames;
    }

    public String appendType(String requestName) {
        return requestInput.getFlows().get(requestName) != null
            ? requestName + " " + FLOW_TAG
            : requestInput.getRequests().get(requestName).menuName() + " " + REQUEST_TAG;
    }

    private String getType(String requestName) {
        if(requestName == null) {
            return null;
        }
        return requestInput.getFlows().get(requestName) != null
            ? FLOW_NAME : REQUEST_NAME;
    }
}
