package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedAlias;
import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedName;

import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.legadi.cli.jurl.common.Settings;

public abstract class Option {

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
        return Objects.hash(extractNamedName(this), extractNamedAlias(this));
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
        return Objects.equals(extractNamedName(this), extractNamedName(other))
                && Objects.equals(extractNamedAlias(this), extractNamedAlias(other));
    }

    @Override
    public String toString() {
        String args = Optional.ofNullable(getArgs())
            .map(Arrays::stream)
            .orElse(Stream.empty())
            .map(arg -> "<" + arg + ">")
            .collect(Collectors.joining(" "));
        String alias = extractNamedAlias(this);
        return extractNamedName(this)
            + (!alias.isEmpty() ? ", " + alias : "")
            + (!args.isEmpty() ? " " + args : "");
    }
}
