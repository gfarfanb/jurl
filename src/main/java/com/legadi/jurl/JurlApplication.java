package com.legadi.jurl;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.options.OptionsProcessor;

public class JurlApplication {

    private static final Logger LOGGER = Logger.getLogger(JurlApplication.class.getName());

    public static void main(String[] args) throws Exception {
        System.setProperty("java.util.logging.SimpleFormatter.format", "%4$s: %5$s%6$s%n");

        try {
            args = new String[] { "--env", "dev", "file" };
            OptionsProcessor optionsProcessor = new OptionsProcessor(args);
            LOGGER.info("specPath: " + optionsProcessor.getSpecPath());
            LOGGER.info("options: " + optionsProcessor.getOptions());
        } catch(CommandException | RequestException ex) {
            LOGGER.log(Level.SEVERE, ex.getMessage(), ex);
            System.exit(1);
        } catch(Exception ex) {
            LOGGER.log(Level.SEVERE, ex.getMessage(), ex);
            System.exit(1);
        }
    }

}
