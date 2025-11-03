package com.legadi.cli.jurl.options;

import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.legadi.cli.jurl.common.Named;
import com.legadi.cli.jurl.common.Settings;

public abstract class Option implements Named {

    @Override
    public boolean allowOverride() {
        return false;
    }

    public boolean requiredForAuth() {
        return false;
    }

    public abstract String[] getArgs();

    public abstract String getDescription();

    public int getPriority() {
        return 0;
    }

    public abstract boolean execute(Settings settings, String[] args);

    @Override
    public int hashCode() {
        return Objects.hash(name(), alias());
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof Option)) {
            return false;
        }
        Option other = (Option) obj;
        return Objects.equals(name(), other.name())
                && Objects.equals(alias(), other.alias());
    }

    @Override
    public String toString() {
        String args = Optional.ofNullable(getArgs())
            .map(Arrays::stream)
            .orElse(Stream.empty())
            .map(arg -> "<" + arg + ">")
            .collect(Collectors.joining(" "));
        return name()
            + (alias() != null ? ", " + alias() : "")
            + (!args.isEmpty() ? " " + args : "");
    }
}
