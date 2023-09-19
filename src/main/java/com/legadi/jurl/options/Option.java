package com.legadi.jurl.options;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Settings;

public abstract class Option {

    public abstract String getOpt();

    public abstract String getAlias();

    public abstract String[] getArgs();

    public abstract String getDescription();

    public int getPriority() {
        return 0;
    }

    public abstract boolean execute(Settings settings, String[] args);

    @Override
    public int hashCode() {
        return Objects.hash(getOpt(), getAlias());
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
        return Objects.equals(getOpt(), other.getOpt())
                && Objects.equals(getAlias(), other.getAlias());
    }

    @Override
    public String toString() {
        return getOpt()
            + (isNotBlank(getAlias()) ? ", " + getAlias() : "")
            + (getArgs().length < 1 ? "" : " " + Arrays.stream(getArgs())
                .map(arg -> "<" + arg + ">")
                .collect(Collectors.joining(" ")));
    }
}