package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotNumeric;
import static com.legadi.cli.jurl.common.JsonUtils.toJsonString;
import static com.legadi.cli.jurl.common.LoaderUtils.instantiate;
import static com.legadi.cli.jurl.common.LoaderUtils.typeOf;
import static com.legadi.cli.jurl.common.WriterUtils.expandFile;
import static java.util.logging.Level.FINE;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.legadi.cli.jurl.common.OutputPathBuilder;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.http.HTTPMockEntry;

public class HTTPMockConnection extends HttpURLConnection {

    private static final Logger LOGGER = Logger.getLogger(HTTPMockConnection.class.getName());

    private final URL url;
    private final Settings settings;
    private final String requestFilePath;
    private final String inputName;
    private final Map<String, Object> defaults;

    private int responseCode;
    private Long secondsDelay;
    private String responseContent;
    private String responseFilePath;
    private Map<String, List<String>> responseHeaders = new HashMap<>();
    private Class<? extends IOException> exceptionClassOnOutputStream;
    private Class<? extends IOException> exceptionClassOnResponseCode;
    private boolean doOutput;

    public HTTPMockConnection(URL url, Settings settings,
            String requestFilePath, String inputName,
            Map<String, Object> defaults,
            HTTPMockEntry mockEntry) {
        super(null);

        this.url = url;
        this.settings = settings;
        this.requestFilePath = requestFilePath;
        this.inputName = inputName;
        this.defaults = defaults;

        initMock(mockEntry);
    }

    private void initMock(HTTPMockEntry mockEntry) {
        if(mockEntry == null) {
            return;
        }

        if(isNotBlank(mockEntry.getStatusCode()) && isNotNumeric(mockEntry.getStatusCode())) {
            throw new CommandException("Mock 'statusCode' must be numeric: " + mockEntry.getStatusCode());
        }
        if(isNotBlank(mockEntry.getSecondsDelay()) && isNotNumeric(mockEntry.getSecondsDelay())) {
            throw new CommandException("Mock 'secondsDelay' must be numeric: " + mockEntry.getSecondsDelay());
        }

        this.responseCode = isNotBlank(mockEntry.getStatusCode())
            ? Integer.parseInt(mockEntry.getStatusCode()) : 0;
        this.secondsDelay = isNotBlank(mockEntry.getSecondsDelay())
            ? Long.parseLong(mockEntry.getSecondsDelay()) : null;
        this.responseContent = mockEntry.getResponseContent();
        this.responseFilePath = mockEntry.getResponseFilePath();

        this.responseHeaders.putAll(mockEntry.getResponseHeaders()
            .entrySet()
            .stream()
            .collect(Collectors.toMap(
                e -> e.getKey(),
                e -> Arrays.asList(e.getValue()))
            ));
        this.responseHeaders.put(null, Arrays.asList("HTTP/1.1 " + responseCode));

        this.exceptionClassOnOutputStream = toIOExceptionClass(mockEntry.getExceptionClassOnOutputStream());
        this.exceptionClassOnResponseCode = toIOExceptionClass(mockEntry.getExceptionClassOnResponseCode());
    }

    @Override
    public URL getURL() {
        return url;
    }

    @Override
    public void disconnect() {
        LOGGER.fine("[mock-connection] Calling disconnect()");
    }

    @Override
    public boolean usingProxy() {
        LOGGER.fine("[mock-connection] Calling usingProxy():false");
        return false;
    }

    @Override
    public void connect() throws IOException {
        LOGGER.fine("[mock-connection] Calling connect()");
    }

    @Override
    public void setRequestMethod(String method) throws ProtocolException {
        LOGGER.fine("[mock-connection] Calling setRequestMethod(" + method + ")");
    }

    @Override
    public void setRequestProperty(String key, String value) {
        LOGGER.fine("[mock-connection] Calling setRequestProperty(" + key + ", " + value + ")");
    }

    @Override
    public void setDoOutput(boolean doOutput) {
        LOGGER.fine("[mock-connection] Calling setDoOutput(" + doOutput + ")");
        this.doOutput = doOutput;
    }

    @Override
    public boolean getDoOutput() {
        LOGGER.fine("[mock-connection] Calling getDoOutput():" + doOutput);
        return doOutput;
    }

    @Override
    public OutputStream getOutputStream() throws IOException {
        if(exceptionClassOnOutputStream != null) {
            LOGGER.fine("[mock-connection] Calling getOutputStream():ByteArrayOutputStream - throwing " + exceptionClassOnOutputStream);
            IOException exception = instantiate(exceptionClassOnOutputStream);
            throw exception;
        } else {
            LOGGER.fine("[mock-connection] Calling getOutputStream():ByteArrayOutputStream");
            return new ByteArrayOutputStream();
        }
    }

    @Override
    public void setDoInput(boolean input) {
        LOGGER.fine("[mock-connection] Calling setDoInput(" + input + ")");
    }

    @Override
    public InputStream getInputStream() throws IOException {
        try {
            TimeUnit.SECONDS.sleep(secondsDelay);
        } catch(Exception ex) {
            LOGGER.log(FINE, "Error on sleeping mock - secondsDelay=" + secondsDelay, ex);
        }

        if(isNotBlank(responseContent)) {
            LOGGER.fine("[mock-connection] Calling getInputStream():ByteArrayInputStream - " + responseContent);
            return new ByteArrayInputStream(responseContent.getBytes());
        }
        if(isNotBlank(responseFilePath)) {
            OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
                .setRequestPath(requestFilePath)
                .setRequestName(inputName)
                .setExtension("mock");
            Path temporalBodyPath = pathBuilder.buildCommandPath();
            Path responseBodyPath = Paths.get(responseFilePath);

            expandFile(settings, responseBodyPath, temporalBodyPath, defaults);

            LOGGER.fine("[mock-connection] Calling getInputStream():ByteArrayInputStream - " + temporalBodyPath);
            return Files.newInputStream(temporalBodyPath);
        }

        LOGGER.fine("[mock-connection] Calling getInputStream():ByteArrayInputStream");
        return new ByteArrayInputStream(new byte[0]);
    }

    @Override
    public int getResponseCode() throws IOException {
        if(exceptionClassOnResponseCode != null) {
            LOGGER.fine("[mock-connection] Calling getResponseCode():int - throwing " + exceptionClassOnResponseCode);
            IOException exception = instantiate(exceptionClassOnResponseCode);
            throw exception;
        } else {
            LOGGER.fine("[mock-connection] Calling getResponseCode():" + responseCode);
            return responseCode;
        }
    }

    @Override
    public Map<String, List<String>> getHeaderFields() {
        LOGGER.fine("[mock-connection] Calling getHeaderFields():" + responseHeaders.getClass().getSimpleName()
            + "-" + toJsonString(responseHeaders));
        return responseHeaders;
    }

    @Override
    public void setUseCaches(boolean useCaches) {
        LOGGER.fine("[mock-connection] Calling setUseCaches(" + useCaches + ")");
    }

    @SuppressWarnings("unchecked")
    private Class<? extends IOException> toIOExceptionClass(String exceptionClass) {
        if(isBlank(exceptionClass)) {
            return null;
        }

        Class<?> type = typeOf(exceptionClass);

        if(IOException.class.isAssignableFrom(type)) {
            return (Class<? extends IOException>) type;
        } else {
            return null;
        }
    }
}
