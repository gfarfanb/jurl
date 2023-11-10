package com.legadi.jurl.options;

import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.legadi.jurl.exception.CommandException;

public class OptionsRegistry {

    private static final Map<String, Supplier<Option>> REGISTERED_OPTIONS = new HashMap<>();
    private static final Map<String, Supplier<Option>> ADD_ON_OPTIONS = new HashMap<>();

    static {
        registerOption(AuthorizationBasicOption::new);
        registerOption(AuthorizationTokenOption::new);
        registerOption(CleanOutputOption::new);
        registerOption(CurlPrintOption::new);
        registerOption(CustomGeneratorOption::new);
        registerOption(CustomHandlerOption::new);
        registerOption(CustomMixerOption::new);
        registerOption(CustomModifierOption::new);
        registerOption(CustomParserOption::new);
        registerOption(CustomReaderOption::new);
        registerOption(EnvironmentOption::new);
        registerOption(HelpOption::new);
        registerOption(MergeBodyOption::new);
        registerOption(MockRequestOption::new);
        registerOption(OpenEditorOption::new);
        registerOption(OverrideRequestOption::new);
        registerOption(RequestPrintOption::new);
        registerOption(RequestTypeOption::new);
        registerOption(SetInputNameOption::new);
        registerOption(SetValueOption::new);
        registerOption(SkipAssertionsOption::new);
        registerOption(SkipConditionsOption::new);
        registerOption(TimesRepeatOption::new);
    }

    private OptionsRegistry() {}

    public static Option registerAddOn(String optionClass) {
        return registerOption(() -> instantiate(optionClass), true);
    }

    public static Option registerOption(Supplier<Option> optionSupplier) {
        return registerOption(optionSupplier, false);
    }

    public static Option registerOption(Supplier<Option> optionSupplier, boolean isAddOn) {
        Option option = optionSupplier.get();

        if(REGISTERED_OPTIONS.containsKey(option.getOpt()) || ADD_ON_OPTIONS.containsKey(option.getOpt())) {
            throw new CommandException("Option [" + option.getOpt() + "] already exists");
        }
        if(REGISTERED_OPTIONS.containsKey(option.getAlias()) || ADD_ON_OPTIONS.containsKey(option.getAlias())) {
            throw new CommandException("Option [" + option.getAlias() + "] already exists");
        }
        if(isBlank(option.getOpt())) {
            throw new CommandException("Option does not have [opt] defined");
        }

        Map<String, Supplier<Option>> registeredOptions;
        if(isAddOn) {
            registeredOptions = ADD_ON_OPTIONS;
        } else {
            registeredOptions = REGISTERED_OPTIONS;
        }

        registeredOptions.put(option.getOpt().toLowerCase(), optionSupplier);

        if(isNotBlank(option.getAlias())) {
            registeredOptions.put(option.getAlias().toLowerCase(), optionSupplier);
        }

        return option;
    }

    public static Set<Option> getOptions() {
        return REGISTERED_OPTIONS.values()
            .stream()
            .map(Supplier::get)
            .collect(Collectors.toSet());
    }

    public static Set<Option> getAddOns() {
        return ADD_ON_OPTIONS.values()
            .stream()
            .map(Supplier::get)
            .collect(Collectors.toSet());
    }

    public static Option findByArg(String arg) {
        Supplier<Option> optionSupplier = REGISTERED_OPTIONS.getOrDefault(
            arg.toLowerCase(), ADD_ON_OPTIONS.get(arg.toLowerCase())
        );

        if(optionSupplier != null) {
            return optionSupplier.get();
        } else {
            return null;
        }
    }
}
