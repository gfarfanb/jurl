package com.legadi.cli.jurl.common;

import static java.util.logging.Level.INFO;

import java.security.GeneralSecurityException;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManager;

public class Setup {

    private static final AtomicReference<Level> LOG_LEVEL = new AtomicReference<>(INFO);

    private Setup() {}

    public static void setupLogLevel() {
        String jurlLogLevel = System.getenv("JURL_LOG_LEVEL");
        Level logLevel = INFO;

        if(jurlLogLevel != null) {
            switch(jurlLogLevel.toUpperCase()) {
                case "OFF": logLevel = Level.OFF; break;
                case "SEVERE": logLevel = Level.SEVERE; break;
                case "WARNING": logLevel = Level.WARNING; break;
                case "INFO": logLevel = Level.INFO; break;
                case "CONFIG": logLevel = Level.CONFIG; break;
                case "FINE": logLevel = Level.FINE; break;
                case "FINER": logLevel = Level.FINER; break;
                case "FINEST": logLevel = Level.FINEST; break;
                case "ALL": logLevel = Level.ALL; break;
                default:
                    throw new IllegalStateException("Invalid log level: " + jurlLogLevel);
            }
        }

        LOG_LEVEL.set(logLevel);

        Logger rootLogger = LogManager.getLogManager().getLogger("");
        decorateLogger(rootLogger);
    }

    private static Logger decorateLogger(Logger logger) {
        logger.setLevel(LOG_LEVEL.get());

        for (Handler handler : logger.getHandlers()) {
            handler.setLevel(LOG_LEVEL.get());
            handler.setFormatter(new LogFormatter());
        }

        return logger;
    }

    public static void setupConnectionstoAcceptAllHosts() throws GeneralSecurityException {
        TrustManager[] trustAllCerts = new TrustManager[] {
            new TrustedManager()
        };

        SSLContext context = SSLContext.getInstance("SSL");
        context.init(null, trustAllCerts, new java.security.SecureRandom());
        HttpsURLConnection.setDefaultSSLSocketFactory(context.getSocketFactory());

        HostnameVerifier hostnameVerifier = new HostnameVerifier() {

            @Override
            public boolean verify(String hostname, SSLSession session) {
                return true;
            }
        };

        HttpsURLConnection.setDefaultHostnameVerifier(hostnameVerifier);
    }
}
