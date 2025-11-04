package com.legadi.cli.jurl.generators;

import static java.util.logging.Level.FINE;

import java.util.Random;
import java.util.logging.Logger;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.CommandException;

public interface Generator {

    public static final Logger LOGGER = Logger.getLogger(Generator.class.getName());

    String getValue(Settings settings, String param);

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

    default Random random() {
        return new Random();
    }
}
