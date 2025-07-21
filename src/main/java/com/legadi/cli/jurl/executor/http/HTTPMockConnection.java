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
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.legadi.cli.jurl.common.OutputPathBuilder;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.StringExpander;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.http.HTTPMockEntry;

public class HTTPMockConnection extends HttpURLConnection {

    private static final Logger LOGGER = Logger.getLogger(HTTPMockConnection.class.getName());

    private final URL url;
    private final Settings settings;
    private final StringExpander stringExpander;
    private final String requestFilePath;
    private final String inputName;
    private final Map<String, Object> defaults;

    private int responseCode;
    private Long secondsDelay;
    private String responseContent;
    private String responseFilePath;
    private Map<String, String> responseHeaders;
    private String exceptionClassOnOutputStream;
    private String exceptionClassOnResponseCode;
    private boolean doOutput;

    public HTTPMockConnection(URL url, Settings settings,
            String requestFilePath, String inputName,
            Map<String, Object> defaults,
            HTTPMockEntry mockEntry) {
        super(null);

        this.url = url;
        this.settings = settings;
        this.stringExpander = new StringExpander(settings, defaults);
        this.requestFilePath = requestFilePath;
        this.inputName = inputName;
        this.defaults = defaults;

        initMock(mockEntry);
    }

    private void initMock(HTTPMockEntry mockEntry) {
        if(mockEntry == null) {
            return;
        }

        String statusCode = stringExpander.replaceAllInContent(mockEntry.getStatusCode());
        if(isNotBlank(statusCode) && isNotNumeric(statusCode)) {
            throw new CommandException("Mock 'statusCode' must be numeric: " + statusCode);
        }
        String secondsDelay = stringExpander.replaceAllInContent(mockEntry.getSecondsDelay());
        if(isNotBlank(secondsDelay) && isNotNumeric(secondsDelay)) {
            throw new CommandException("Mock 'secondsDelay' must be numeric: " + secondsDelay);
        }

        this.responseCode = isNotBlank(statusCode) ? Integer.parseInt(statusCode) : 0;
        this.secondsDelay = isNotBlank(secondsDelay) ? Long.parseLong(secondsDelay) : null;
        this.responseContent = mockEntry.getResponseContent();
        this.responseFilePath = mockEntry.getResponseFilePath();

        this.responseHeaders = mockEntry.getResponseHeaders();
        this.responseHeaders.put(null, "HTTP/1.1 " + responseCode);

        this.exceptionClassOnOutputStream = mockEntry.getExceptionClassOnOutputStream();
        this.exceptionClassOnResponseCode = mockEntry.getExceptionClassOnResponseCode();
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
        exceptionClassOnOutputStream = stringExpander.replaceAllInContent(exceptionClassOnOutputStream);
        Class<? extends IOException> exceptionClass = toIOExceptionClass(exceptionClassOnOutputStream);

        if(exceptionClass != null) {
            LOGGER.fine("[mock-connection] Calling getOutputStream():ByteArrayOutputStream - throwing " + exceptionClass);
            IOException exception = instantiate(exceptionClass);
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

        responseContent = stringExpander.replaceAllInContent(responseContent);
        if(isNotBlank(responseContent)) {
            LOGGER.fine("[mock-connection] Calling getInputStream():ByteArrayInputStream - " + responseContent);
            return new ByteArrayInputStream(responseContent.getBytes());
        }

        responseFilePath = stringExpander.replaceAllInContent(responseFilePath);
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
        exceptionClassOnResponseCode = stringExpander.replaceAllInContent(exceptionClassOnResponseCode);
        Class<? extends IOException> exceptionClass = toIOExceptionClass(exceptionClassOnResponseCode);

        if(exceptionClass != null) {
            LOGGER.fine("[mock-connection] Calling getResponseCode():int - throwing " + exceptionClass);
            IOException exception = instantiate(exceptionClass);
            throw exception;
        } else {
            LOGGER.fine("[mock-connection] Calling getResponseCode():" + responseCode);
            return responseCode;
        }
    }

    @Override
    public Map<String, List<String>> getHeaderFields() {
        Map<String, List<String>> headerFields = Optional.ofNullable(responseHeaders)
            .orElse(new HashMap<>())
            .entrySet()
            .stream()
            .collect(Collectors.toMap(
                e -> e.getKey(),
                e -> Arrays.asList(stringExpander.replaceAllInContent(e.getValue())))
            );

        LOGGER.fine("[mock-connection] Calling getHeaderFields():" + headerFields.getClass().getSimpleName()
            + "-" + toJsonString(headerFields));
        return headerFields;
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
