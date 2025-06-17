package com.legadi.cli.jurl;

import static com.legadi.cli.jurl.common.Setup.setupConnectionstoAcceptAllHosts;
import static com.legadi.cli.jurl.common.Setup.setupLogLevel;
import static java.util.logging.Level.FINE;
import static java.util.logging.Level.SEVERE;

import java.util.logging.Logger;

import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.exception.InvalidAssertionsFoundException;
import com.legadi.cli.jurl.exception.RequestException;
import com.legadi.cli.jurl.exception.SkipExecutionException;
import com.legadi.cli.jurl.executor.RequestCommand;

public class JurlApplication {

    private static final Logger LOGGER = Logger.getLogger(JurlApplication.class.getName());

    public static void main(String[] args) throws Exception {
        try {
            setupLogLevel();
            setupConnectionstoAcceptAllHosts();

            new RequestCommand(args).execute();
        } catch(SkipExecutionException | InvalidAssertionsFoundException ex) {
            LOGGER.log(FINE, ex.getMessage(), ex);
        } catch(CommandException | RequestException ex) {
            LOGGER.log(SEVERE, ex.getMessage());
        } catch(Exception ex) {
            LOGGER.log(SEVERE, "Unrecognized error: " + ex.getMessage());
            LOGGER.log(FINE, ex.getMessage(), ex);
        }
    }
}
