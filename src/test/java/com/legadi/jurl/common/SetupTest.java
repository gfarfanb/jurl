package com.legadi.jurl.common;

import static com.legadi.jurl.common.Setup.setupConnectionstoAcceptAllHosts;
import static com.legadi.jurl.common.Setup.setupLogLevel;
import static java.util.logging.Level.INFO;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.logging.Logger;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

public class SetupTest {

    private static Map<String, String> MODIFIABLE_ENVIRONMENT;

    @BeforeAll
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static void setupAll() throws Exception {
        Class<?> pe = Class.forName("java.lang.ProcessEnvironment");
        Method getenv = pe.getDeclaredMethod("getenv");
        getenv.setAccessible(true);
        Object unmodifiableEnvironment = getenv.invoke(null);
        Class<?> map = Class.forName("java.util.Collections$UnmodifiableMap");
        Field m = map.getDeclaredField("m");
        m.setAccessible(true);
        MODIFIABLE_ENVIRONMENT = (Map) m.get(unmodifiableEnvironment);;
    }

    @AfterEach
    public void cleanup() {
        MODIFIABLE_ENVIRONMENT.remove("JURL_LOG_LEVEL");
    }

    @Test
    public void logMessage() {
        setupLogLevel();

        Logger logger = Logger.getLogger(SetupTest.class.getName());

        logger.log(INFO, "Log message");
        logger.log(INFO, "Log Message", new RuntimeException());
    }

    @Test
    public void setupLogLevelValidation() {
        setupLogLevel();
        MODIFIABLE_ENVIRONMENT.put("JURL_LOG_LEVEL", "OFF");
        setupLogLevel();
        MODIFIABLE_ENVIRONMENT.put("JURL_LOG_LEVEL", "SEVERE");
        setupLogLevel();
        MODIFIABLE_ENVIRONMENT.put("JURL_LOG_LEVEL", "WARNING");
        setupLogLevel();
        MODIFIABLE_ENVIRONMENT.put("JURL_LOG_LEVEL", "INFO");
        setupLogLevel();
        MODIFIABLE_ENVIRONMENT.put("JURL_LOG_LEVEL", "CONFIG");
        setupLogLevel();
        MODIFIABLE_ENVIRONMENT.put("JURL_LOG_LEVEL", "FINE");
        setupLogLevel();
        MODIFIABLE_ENVIRONMENT.put("JURL_LOG_LEVEL", "FINER");
        setupLogLevel();
        MODIFIABLE_ENVIRONMENT.put("JURL_LOG_LEVEL", "FINEST");
        setupLogLevel();
        MODIFIABLE_ENVIRONMENT.put("JURL_LOG_LEVEL", "ALL");
        setupLogLevel();
    }

    @Test
    public void setupLogLevelInvalidLevel() {
        MODIFIABLE_ENVIRONMENT.put("JURL_LOG_LEVEL", "INVALID");
        Assertions.assertThrows(IllegalStateException.class,
            () -> setupLogLevel());
    }

    @Test
    public void setupConnectionstoAcceptAllHostsValidation() throws IOException {
        Assertions.assertDoesNotThrow(() -> setupConnectionstoAcceptAllHosts());

        HostnameVerifier hostnameVerifier = HttpsURLConnection.getDefaultHostnameVerifier();

        Assertions.assertTrue(hostnameVerifier.verify(null, null));
    }
}
