package com.legadi.jurl.executor.http;

import static com.legadi.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.jurl.common.SettingsConstants.PROP_MERGE_BODY_USING_TYPE;
import static com.legadi.jurl.common.WriterUtils.writeFile;
import static com.legadi.jurl.executor.http.HTTPRequestModifier.BODY_TEMPORAL_PATH;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.OutputPathBuilder;
import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.executor.RequestModifier;
import com.legadi.jurl.model.AssertionEntry;
import com.legadi.jurl.model.RequestInput;
import com.legadi.jurl.model.http.HTTPMockEntry;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.jurl.options.Option;
import com.legadi.jurl.options.OptionsReader.OptionEntry;

public class HTTPRequestModifierTest {

    @Test
    public void appendAuthenticationDefinitionValidation() {
        Settings settings = new Settings();
        RequestInput<HTTPRequestEntry> requestInput = new RequestInput<>();
        HTTPRequestEntry request = new HTTPRequestEntry();
        List<OptionEntry> options = new LinkedList<>();
        String inputName = UUID.randomUUID().toString();
        String requestName = UUID.randomUUID().toString();

        request.setName(requestName);
        requestInput.setApi(new HTTPRequestEntry());
        requestInput.getRequests().put(inputName, request);

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        Pair<String, RequestInput<?>> input = modifier.appendAuthenticationIfExists(settings, requestInput, options);

        Assertions.assertEquals(inputName, input.getLeft());
        Assertions.assertFalse(input.getRight().getRequests().isEmpty());
        Assertions.assertNotNull(input.getRight().getRequests().get(inputName));
        Assertions.assertEquals(requestName, input.getRight().getRequests().get(inputName).getName());
    }

    @Test
    public void mergeRequestHeaderValidation() {
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setUrl("https://localhost:9876");
        api.setProtocol("https");
        api.setPort("9876");
        api.setHost("localhost:9876");
        api.setBasePath("/api");
        api.setEndpoint("/v1");

        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setUrl("http://localhost:1234");
        request.setProtocol("http");
        request.setHost("localhost:1234");
        request.setPort("1234");
        request.setBasePath("/base");
        request.setEndpoint("/endpoint");

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeHeader(api, request);

        Assertions.assertEquals("http://localhost:1234", request.getUrl());
        Assertions.assertEquals("http", request.getProtocol());
        Assertions.assertEquals("localhost:1234", request.getHost());
        Assertions.assertEquals("1234", request.getPort());
        Assertions.assertEquals("/base", request.getBasePath());
        Assertions.assertEquals("/endpoint", request.getEndpoint());
    }

    @Test
    public void mergeRequestHeaderAPIValues() {
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setUrl("https://localhost:9876");
        api.setProtocol("https");
        api.setHost("localhost:9876");
        api.setPort("9876");
        api.setBasePath("/api");
        api.setEndpoint("/v1");

        HTTPRequestEntry request = new HTTPRequestEntry();

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeHeader(api, request);

        Assertions.assertEquals("https://localhost:9876", request.getUrl());
        Assertions.assertEquals("https", request.getProtocol());
        Assertions.assertEquals("localhost:9876", request.getHost());
        Assertions.assertEquals("9876", request.getPort());
        Assertions.assertEquals("/api", request.getBasePath());
        Assertions.assertEquals("/v1", request.getEndpoint());
    }

    @Test
    public void mergeAPIDefinitionValidation() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setMockDefinition(new HTTPMockEntry());

        AssertionEntry assertionEntry = new AssertionEntry();
        assertionEntry.setName("TEST");

        api.getConditions().add(assertionEntry);
        api.getOutputMappings().put("mapping.property", "{{mapping}}");
        api.getAssertions().add(assertionEntry);

        OptionEntry option = new OptionEntry(findByNameOrFail(Option.class, "-h"), new String[0]);

        api.getOptions().add(option);

        api.setMethod("POST");
        api.getQueryParams().put("param", "param-value");
        api.getHeaders().put("header", "header-value");
        api.setBodyCharset(StandardCharsets.UTF_16.name());
        api.setBodyContent("{}");
        api.setBodyFilePath("path/");
        api.setRequestFile(new HTTPRequestFileEntry());

        HTTPRequestEntry request = new HTTPRequestEntry();

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertNotNull(request.getMockDefinition());
        Assertions.assertFalse(request.getConditions().isEmpty());
        Assertions.assertEquals("TEST", request.getConditions().get(0).getName());
        Assertions.assertFalse(request.getOutputMappings().isEmpty());
        Assertions.assertEquals("{{mapping}}", request.getOutputMappings().get("mapping.property"));
        Assertions.assertFalse(request.getAssertions().isEmpty());
        Assertions.assertEquals("TEST", request.getAssertions().get(0).getName());
        Assertions.assertFalse(request.getOptions().isEmpty());
        Assertions.assertEquals("--help", request.getOptions().get(0).getLeft().name());

        Assertions.assertEquals("POST", request.getMethod());
        Assertions.assertFalse(request.getQueryParams().isEmpty());
        Assertions.assertEquals("param-value", request.getQueryParams().get("param"));
        Assertions.assertFalse(request.getHeaders().isEmpty());
        Assertions.assertEquals("header-value", request.getHeaders().get("header"));
        Assertions.assertEquals(StandardCharsets.UTF_16.name(), request.getBodyCharset());
        Assertions.assertEquals("{}", request.getBodyContent());
        Assertions.assertEquals("path/", request.getBodyFilePath());
        Assertions.assertNotNull(request.getRequestFile());
    }

    @Test
    public void mergeAPIDefinitionRequestValues() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();
        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setMockDefinition(new HTTPMockEntry());
        request.setMethod("POST");
        request.setBodyCharset(StandardCharsets.UTF_16.name());
        request.setBodyContent("{}");
        request.setBodyFilePath("path/");
        request.setRequestFile(new HTTPRequestFileEntry());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertNotNull(request.getMockDefinition());
        Assertions.assertEquals("POST", request.getMethod());
        Assertions.assertEquals(StandardCharsets.UTF_16.name(), request.getBodyCharset());
        Assertions.assertEquals("{}", request.getBodyContent());
        Assertions.assertEquals("path/", request.getBodyFilePath());
        Assertions.assertNotNull(request.getRequestFile());
    }

    @Test
    public void mergeAPIDefinitionMockDefinition() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setMockDefinition(new HTTPMockEntry());
        api.getMockDefinition().setStatusCode("200");
        api.getMockDefinition().setSecondsDelay("5");
        api.getMockDefinition().getResponseHeaders().put("header", "header-value");
        api.getMockDefinition().setResponseContent("{}");
        api.getMockDefinition().setResponseFilePath("path/");
        api.getMockDefinition().setExceptionClassOnOutputStream(IOException.class.getName());
        api.getMockDefinition().setExceptionClassOnResponseCode(IOException.class.getName());

        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setMockDefinition(new HTTPMockEntry());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertEquals("200", request.getMockDefinition().getStatusCode());
        Assertions.assertEquals("5", request.getMockDefinition().getSecondsDelay());
        Assertions.assertFalse(request.getMockDefinition().getResponseHeaders().isEmpty());
        Assertions.assertEquals("header-value", request.getMockDefinition().getResponseHeaders().get("header"));
        Assertions.assertEquals("{}", request.getMockDefinition().getResponseContent());
        Assertions.assertEquals("path/", request.getMockDefinition().getResponseFilePath());
        Assertions.assertEquals(IOException.class.getName(), request.getMockDefinition().getExceptionClassOnOutputStream());
        Assertions.assertEquals(IOException.class.getName(), request.getMockDefinition().getExceptionClassOnResponseCode());
    }

    @Test
    public void mergeAPIDefinitionMockDefinitionRequestValues() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setMockDefinition(new HTTPMockEntry());

        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setMockDefinition(new HTTPMockEntry());
        request.getMockDefinition().setStatusCode("200");
        request.getMockDefinition().setSecondsDelay("5");
        request.getMockDefinition().getResponseHeaders().put("header", "header-value");
        request.getMockDefinition().setResponseContent("{}");
        request.getMockDefinition().setResponseFilePath("path/");
        request.getMockDefinition().setExceptionClassOnOutputStream(IOException.class.getName());
        request.getMockDefinition().setExceptionClassOnResponseCode(IOException.class.getName());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertEquals("200", request.getMockDefinition().getStatusCode());
        Assertions.assertEquals("5", request.getMockDefinition().getSecondsDelay());
        Assertions.assertFalse(request.getMockDefinition().getResponseHeaders().isEmpty());
        Assertions.assertEquals("header-value", request.getMockDefinition().getResponseHeaders().get("header"));
        Assertions.assertEquals("{}", request.getMockDefinition().getResponseContent());
        Assertions.assertEquals("path/", request.getMockDefinition().getResponseFilePath());
        Assertions.assertEquals(IOException.class.getName(), request.getMockDefinition().getExceptionClassOnOutputStream());
        Assertions.assertEquals(IOException.class.getName(), request.getMockDefinition().getExceptionClassOnResponseCode());
    }

    @Test
    public void mergeAPIDefinitionRequestFile() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setRequestFile(new HTTPRequestFileEntry());
        api.getRequestFile().setName("file");
        api.getRequestFile().setPath("path/");
        api.getRequestFile().setField("field");
        api.getRequestFile().setMineType("application/xml");
        api.getRequestFile().getFormData().put("field", "field-value");

        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setRequestFile(new HTTPRequestFileEntry());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertEquals("file", request.getRequestFile().getName());
        Assertions.assertEquals("path/", request.getRequestFile().getPath());
        Assertions.assertEquals("field", request.getRequestFile().getField());
        Assertions.assertEquals("application/xml", request.getRequestFile().getMineType());
        Assertions.assertFalse(request.getRequestFile().getFormData().isEmpty());
        Assertions.assertEquals("field-value", request.getRequestFile().getFormData().get("field"));
    }

    @Test
    public void mergeAPIDefinitionRequestFileRequestValues() {
        Settings settings = new Settings();
        HTTPRequestEntry api = new HTTPRequestEntry();

        api.setRequestFile(new HTTPRequestFileEntry());

        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setRequestFile(new HTTPRequestFileEntry());
        request.getRequestFile().setName("file");
        request.getRequestFile().setPath("path/");
        request.getRequestFile().setField("field");
        request.getRequestFile().setMineType("application/xml");
        request.getRequestFile().getFormData().put("field", "field-value");

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeAPI(settings, api, request);

        Assertions.assertEquals("file", request.getRequestFile().getName());
        Assertions.assertEquals("path/", request.getRequestFile().getPath());
        Assertions.assertEquals("field", request.getRequestFile().getField());
        Assertions.assertEquals("application/xml", request.getRequestFile().getMineType());
        Assertions.assertFalse(request.getRequestFile().getFormData().isEmpty());
        Assertions.assertEquals("field-value", request.getRequestFile().getFormData().get("field"));
    }

    @Test
    public void mergeBodyFileWithBodyContentValidation() {
        Settings settings = new Settings();

        settings.putOverride(PROP_MERGE_BODY_USING_TYPE, "json");

        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
                .setRequestPath("src/test/resources/http-request-modifier.http")
                .setRequestName("merge-body")
                .setExtension("content");
        Path bodyPath = pathBuilder.buildCommandPath();

        writeFile(bodyPath, "{\"name\": \"file-content\"}");

        HTTPRequestEntry request = new HTTPRequestEntry();
        request.setName("merge-body");
        request.setBodyContent("{\"name\": \"body-content\"}");
        request.setBodyFilePath(bodyPath.toString());

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.mergeBody(settings, "src/test/resources/http-request-modifier.http", request);

        Assertions.assertNull(request.getBodyContent());
        Assertions.assertNull(request.getBodyFilePath());

        String bodyTemporalPath = settings.get(BODY_TEMPORAL_PATH);

        Assertions.assertDoesNotThrow(() -> Paths.get(bodyTemporalPath));

        Map<String, Object> merged = loadJsonFile(bodyTemporalPath, new TypeToken<Map<String, Object>>() {});

        Assertions.assertTrue(merged.containsKey("name"));
        Assertions.assertEquals("body-content", merged.get("name"));
    }

    @Test
    public void mergeBodyFileWithBodyContentMissingData() {
        Settings settings = new Settings();

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        HTTPRequestEntry request = new HTTPRequestEntry();

        Assertions.assertThrows(RequestException.class,
            () -> modifier.mergeBody(settings, "src/test/resources/http-request-modifier.http", request));

        request.setBodyFilePath("body/file/path");

        Assertions.assertThrows(RequestException.class,
            () -> modifier.mergeBody(settings, "src/test/resources/http-request-modifier.http", request));
    }

    @Test
    public void overrideRequestWithFileValidation() {
        Settings settings = new Settings();
        HTTPRequestEntry request = new HTTPRequestEntry();

        request.getHeaders().put("Content-Type", "application/json");
        request.getQueryParams().put("param", "param-value");
        request.setBodyContent("{}");
        request.setBodyFilePath("path/");

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.overrideRequest(settings, request, "src/test/resources/http-request-modifier.request.txt");

        Assertions.assertEquals(1, request.getHeaders().size());
        Assertions.assertEquals("application/xml", request.getHeaders().get("Content-Type"));
        Assertions.assertEquals(1, request.getQueryParams().size());
        Assertions.assertDoesNotThrow(() -> UUID.fromString(request.getQueryParams().get("param")));
        Assertions.assertTrue(request.getBodyContent().contains("\"name\": \"request\""));
        Assertions.assertEquals("overrided/request/path", request.getBodyFilePath());
    }

    @Test
    public void overrideRequestWithFileEmpty() {
        Settings settings = new Settings();
        HTTPRequestEntry request = new HTTPRequestEntry();

        request.getHeaders().put("Content-Type", "application/json");
        request.getQueryParams().put("param", "param-value");
        request.setBodyContent("{}");
        request.setBodyFilePath("path/");

        RequestModifier<?, ?> modifier = findByNameOrFail(RequestModifier.class, "http");
        modifier.overrideRequest(settings, request, "src/test/resources/http-request-modifier.empty.txt");

        Assertions.assertEquals(1, request.getHeaders().size());
        Assertions.assertEquals("application/json", request.getHeaders().get("Content-Type"));
        Assertions.assertEquals(1, request.getQueryParams().size());
        Assertions.assertEquals("param-value", request.getQueryParams().get("param"));
        Assertions.assertEquals("{}", request.getBodyContent());
        Assertions.assertEquals("path/", request.getBodyFilePath());
    }
}
