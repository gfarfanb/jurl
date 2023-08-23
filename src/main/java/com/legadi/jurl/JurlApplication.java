package com.legadi.jurl;

import static java.util.logging.Level.SEVERE;

import java.util.logging.Logger;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.exception.SkipExecutionException;
import com.legadi.jurl.executor.RequestProcessor;

public class JurlApplication {

    private static final Logger LOGGER = Logger.getLogger(JurlApplication.class.getName());

    public static void main(String[] args) throws Exception {
        args = new String[] { "-h" , "a"};

        try {
            System.setProperty("java.util.logging.SimpleFormatter.format", "%5$s%6$s%n");
            System.setProperty("java.util.logging.ConsoleHandler.level", "INFO");

            new RequestProcessor(args).execute();
        } catch(SkipExecutionException ex) {
            System.exit(0);
        } catch(CommandException | RequestException ex) {
            LOGGER.log(SEVERE, ex.getMessage());
            System.exit(1);
        } catch(Exception ex) {
            LOGGER.log(SEVERE, ex.getMessage(), ex);
            System.exit(1);
        }
    }

}
