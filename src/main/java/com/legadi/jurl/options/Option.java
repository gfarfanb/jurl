package com.legadi.jurl.options;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Named;
import com.legadi.jurl.common.Settings;

public abstract class Option implements Named {

    @Override
    public boolean allowOverride() {
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
        return name()
            + (isNotBlank(alias()) ? ", " + alias() : "")
            + (getArgs().length < 1 ? "" : " " + Arrays.stream(getArgs())
                .map(arg -> "<" + arg + ">")
                .collect(Collectors.joining(" ")));
    }
}
