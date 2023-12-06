package com.legadi.jurl;

import static com.legadi.jurl.common.Setup.setupConnectionstoAcceptAllHosts;
import static com.legadi.jurl.common.Setup.setupLogLevel;
import static java.util.logging.Level.SEVERE;

import java.util.logging.Logger;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.InvalidAssertionsFoundException;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.exception.SkipExecutionException;
import com.legadi.jurl.executor.RequestCommand;

public class JurlApplication {

    private static final Logger LOGGER = Logger.getLogger(JurlApplication.class.getName());

    public static void main(String[] args) throws Exception {
        try {
            setupLogLevel();
            setupConnectionstoAcceptAllHosts();

            new RequestCommand(args).execute();
        } catch(SkipExecutionException ex) {
            System.exit(0);
        } catch(InvalidAssertionsFoundException ex) {
            System.exit(1);
        } catch(CommandException | RequestException ex) {
            LOGGER.log(SEVERE, ex.getMessage());
            System.exit(1);
        } catch(Exception ex) {
            LOGGER.log(SEVERE, ex.getMessage(), ex);
            System.exit(1);
        }
    }
}
