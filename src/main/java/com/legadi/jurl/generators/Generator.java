package com.legadi.jurl.generators;

import static java.util.logging.Level.FINE;

import java.util.Random;
import java.util.logging.Logger;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;

public interface Generator {

    public static final Logger LOGGER = Logger.getLogger(Generator.class.getName());

    String tag();

    String getValue(Settings settings, String param);

    default boolean accepts(String param) {
        return param.startsWith(tag());
    }

    default String get(Settings settings, String param) {
        try {
            return getValue(settings, param);
        } catch(Exception ex) {
            LOGGER.log(FINE, "Unable to generate value from: "
                + param + " - " + ex.getMessage(), ex);
            throw new CommandException("Unable to generate value from: "
                + param + " - " + ex.getMessage());
        }
    }

    default String extractArg(String param) {
        return param.substring(tag().length(), param.length());
    }

    default Random random() {
        return new Random();
    }
}
