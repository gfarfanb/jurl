package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;

import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.RequestInput;

public class InputNameResolver {

    private static final Logger LOGGER = Logger.getLogger(InputNameResolver.class.getName());

    public static final int START_INDEX = 1;
    public static final int INVALID_INDEX = -1;

    private final String requestInputPath;
    private final RequestInput<?> requestInput;

    public InputNameResolver(String requestInputPath, RequestInput<?> requestInput) {
        this.requestInputPath = requestInputPath;
        this.requestInput = requestInput;
    }

    public String resolve(String inputName) {
        if(isNotBlank(inputName)) {
            return inputName;
        }

        List<String> requestNames = listRequestNames();
        Pair<String, Integer> menu = toMenu(requestNames);
        int selectedIndex = Optional.ofNullable(System.console())
                .map(console -> console.readLine(menu.getLeft()))
                .filter(CommonUtils::isNotBlank)
                .filter(CommonUtils::isNumeric)
                .map(Integer::parseInt)
                .orElse(menu.getRight());
        int requestIndex = selectedIndex - START_INDEX;

        if(requestIndex > INVALID_INDEX && requestIndex < requestNames.size()) {
            String requestName = requestNames.get(requestIndex);

            LOGGER.info("Selected: [" + getType(requestName)  +  "] " + requestName + " - " + requestInputPath);
            LOGGER.info("");

            return requestName;
        } else {
            throw new CommandException("Invalid request index: " + selectedIndex
                + " - " + requestInputPath);
        }
    }

    private List<String> listRequestNames() {
        List<String> requestNames = new ArrayList<>();
        requestNames.addAll(requestInput.getRequests().keySet());
        requestNames.addAll(requestInput.getFlows().keySet());
        requestNames.sort(Comparator.naturalOrder());
        return requestNames;
    }

    private Pair<String, Integer> toMenu(List<String> requestNames) {
        StringBuilder menu = new StringBuilder();
        int defaultIndex = INVALID_INDEX;
        int i = START_INDEX;

        for(String requestName : requestNames) {
            if(requestName.equalsIgnoreCase(requestInput.getDefaultRequest())) {
                defaultIndex = i;
            }

            menu
                .append(i)
                .append(") ")
                .append(requestName)
                .append(" [")
                .append(getType(requestName))
                .append("]\n");

            i++;
        }

        if(defaultIndex != INVALID_INDEX) {
            menu
                .append("Select request index [default: ")
                .append(defaultIndex)
                .append("]> ");
        } else {
            menu
                .append("Select request index> ");
        }

        return new Pair<>(menu.toString(), defaultIndex);
    }

    private String getType(String requestName) {
        return requestInput.getFlows().containsKey(requestName)
            ? "flow" : "request";
    }
}
