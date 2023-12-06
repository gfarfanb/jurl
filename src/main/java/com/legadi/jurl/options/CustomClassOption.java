package com.legadi.jurl.options;

import static com.legadi.jurl.common.LoaderUtils.typeOf;
import static com.legadi.jurl.common.ObjectsRegistry.getGroupClasses;
import static com.legadi.jurl.common.ObjectsRegistry.isSubClassOf;
import static com.legadi.jurl.common.ObjectsRegistry.register;

import java.util.stream.Collectors;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;

public class CustomClassOption extends Option {

    @Override
    public String name() {
        return "--custom-class";
    }

    @Override
    public String alias() {
        return "-cc";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "custom-class" };
    }

    @Override
    public String getDescription() {
        return "Registers a custom class.\nIf there is an existing class that accepts the\nsame input the last one will be taken.\nThese are the allowed types:\n"
            + getGroupClasses().stream().map(Class::getName).sorted().collect(Collectors.joining("\n"));
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        Class<?> customClass = typeOf(args[0]);
        register(getGroupClass(customClass), customClass);
        return true;
    }

    private Class<?> getGroupClass(Class<?> typeClass) {
        for(Class<?> expectedType : getGroupClasses()) {
            if(isSubClassOf(expectedType, typeClass)) {
                return expectedType;
            }
        }
        throw new CommandException("Type not allowed: " + typeClass + "\nIt must be a sub-type of:\n"
            + getGroupClasses().stream().map(Class::getName).sorted().collect(Collectors.joining("\n")));
    }
}
