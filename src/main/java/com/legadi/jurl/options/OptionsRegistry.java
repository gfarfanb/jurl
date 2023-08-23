package com.legadi.jurl.options;

import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.legadi.jurl.exception.CommandException;

import static com.legadi.jurl.common.StringUtils.isBlank;
import static com.legadi.jurl.common.StringUtils.isNotBlank;

public class OptionsRegistry {

    private static final Map<String, Option> REGISTERED_OPTIONS = new HashMap<>();
    private static final Map<String, Option> ADD_ON_OPTIONS = new HashMap<>();

    public static Option registerAddOn(Class<Option> optionClass) {
        try {
            Constructor<Option> constructor = optionClass.getConstructor();
            return registerOption(constructor.newInstance(), true);
        } catch(Exception ex) {
            throw new IllegalStateException("Unable to instance option from: " + optionClass, ex);
        }
    }

    public static Option registerOption(Option option) {
        return registerOption(option, false);
    }

    private static Option registerOption(Option option, boolean isAddOn) {
        if(REGISTERED_OPTIONS.containsKey(option.getOpt()) || ADD_ON_OPTIONS.containsKey(option.getOpt())) {
            throw new CommandException("Option [" + option.getOpt() + "] already exists");
        }
        if(REGISTERED_OPTIONS.containsKey(option.getAlias()) || ADD_ON_OPTIONS.containsKey(option.getAlias())) {
            throw new CommandException("Option [" + option.getAlias() + "] already exists");
        }
        if(isBlank(option.getOpt())) {
            throw new CommandException("Option does not have [opt] defined");
        }

        Map<String, Option> registeredOptions;
        if(isAddOn) {
            registeredOptions = ADD_ON_OPTIONS;
        } else {
            registeredOptions = REGISTERED_OPTIONS;
        }

        registeredOptions.put(option.getOpt().toLowerCase(), option);

        if(isNotBlank(option.getAlias())) {
            registeredOptions.put(option.getAlias().toLowerCase(), option);
        }

        return option;
    }

    public static Set<Option> getOptions() {
        return new HashSet<>(REGISTERED_OPTIONS.values());
    }

    public static Set<Option> getAddOns() {
        return new HashSet<>(ADD_ON_OPTIONS.values());
    }

    public static Option getOptionByArg(String arg) {
        return REGISTERED_OPTIONS.getOrDefault(arg.toLowerCase(), ADD_ON_OPTIONS.get(arg.toLowerCase()));
    }
}
