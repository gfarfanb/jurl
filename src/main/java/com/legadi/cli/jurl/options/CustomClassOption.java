package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.LoaderUtils.typeOf;
import static com.legadi.cli.jurl.common.ObjectsRegistry.getGroupClasses;
import static com.legadi.cli.jurl.common.ObjectsRegistry.isSubClassOf;
import static com.legadi.cli.jurl.common.ObjectsRegistry.register;

import java.util.stream.Collectors;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.CommandException;

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
        return "Registers a custom class. If there is an existing class that accepts the same input the last one will be taken. These are the allowed types:"
            + "\n" + getGroupClasses().stream().map(Class::getName).sorted().collect(Collectors.joining("\n"));
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
        throw new CommandException("Type not allowed: " + typeClass + "\nIt must be a sub-type of:"
            + "\n" + getGroupClasses().stream().map(Class::getName).sorted().collect(Collectors.joining("\n")));
    }
}
