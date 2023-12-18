package com.legadi.jurl.common;

import static java.util.logging.Level.INFO;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.security.GeneralSecurityException;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManager;

public class Setup {

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

        Logger rootLogger = LogManager.getLogManager().getLogger("");
        rootLogger.setLevel(logLevel);

        for (Handler handler : rootLogger.getHandlers()) {
            handler.setLevel(logLevel);
            handler.setFormatter(new Formatter() {

                private static final String FORMAT = "%1$s%2$s%n";

                @Override
                public synchronized String format(LogRecord record) {
                    String message = formatMessage(record);
                    String throwable = "";
                    if (record.getThrown() != null) {
                        StringWriter sw = new StringWriter();
                        PrintWriter pw = new PrintWriter(sw);
                        pw.println();
                        record.getThrown().printStackTrace(pw);
                        pw.close();
                        throwable = sw.toString();
                    }
                    return String.format(FORMAT, message, throwable);
                }
            });
        }
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
