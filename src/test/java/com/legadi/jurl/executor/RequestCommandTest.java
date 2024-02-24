package com.legadi.jurl.executor;

import static com.legadi.jurl.common.JsonUtils.jsonToObject;
import static com.legadi.jurl.model.RequestBehaviour.CURL_ONLY;
import static com.legadi.jurl.model.RequestBehaviour.PRINT_ONLY;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.DummyAPITest;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.InvalidAssertionsFoundException;
import com.legadi.jurl.exception.RecursiveCommandException;
import com.legadi.jurl.exception.SkipExecutionException;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.http.HTTPRequestEntry;

public class RequestCommandTest extends DummyAPITest {

    @AfterEach
    public void cleanup() {
        Map<String, String> properties = new HashMap<>();
        properties.put("openEditorCommand", "");
        Settings.mergeProperties("default", properties);
    }

    @Test
    public void executeValidation() {
        String[] args = { "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, "request");

        Assertions.assertEquals(1, requests.size());

        for(HTTPRequestEntry request : requests) {
            Assertions.assertEquals("create", request.getName());
        }
    }

    @Test
    public void executeSkipExecution() {
        String[] args = { "-h" };

        Assertions.assertThrows(SkipExecutionException.class,
            () -> new RequestCommand(args).execute());
    }

    @Test
    public void executeNoRequestInputPath() {
        String[] args = { "-s", "field", "value" };

        Assertions.assertThrows(CommandException.class,
            () -> new RequestCommand(args).execute());
    }

    @Test
    public void executeTimes() {
        String[] args = { "-t", "2", "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, "request");

        Assertions.assertEquals(2, requests.size());

        for(HTTPRequestEntry request : requests) {
            Assertions.assertEquals("create", request.getName());
        }
    }

    @Test
    public void executeDefaultTimes() {
        String[] args = { "-t", "0", "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, "request");

        Assertions.assertEquals(1, requests.size());

        for(HTTPRequestEntry request : requests) {
            Assertions.assertEquals("create", request.getName());
        }
    }

    @Test
    public void executeNoInputName() {
        String[] args = { "src/test/resources/basic-functions.spec.http" };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, "request");

        Assertions.assertEquals(1, requests.size());
    }

    @Test
    public void executeWithException() {
        String[] args = { "-n", "create", "src/test/resources/basic-functions.spec.http" };

        requestCatcher.add(correlationId, "request-with-exception", "create");
        requestCatcher.add(correlationId, "request-with-exception-throw", new CommandException("create"));

        Assertions.assertThrows(CommandException.class,
            () -> new RequestCommand(args).execute());
    }

    @Test
    public void executeWithAuth() {
        String[] args = { "-t", "2", "src/test/resources/auth-request.spec.http" };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, "request");

        Assertions.assertEquals(3, requests.size());

        Set<String> requestNames = new HashSet<>();
        requestNames.add("authorization");
        requestNames.add("create");

        for(HTTPRequestEntry request : requests) {
            Assertions.assertTrue(requestNames.contains(request.getName()));
        }
    }

    @Test
    public void executeSkipAuth() {
        String[] args = { "-na", "src/test/resources/auth-request.spec.http" };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, "request");

        Assertions.assertEquals(1, requests.size());
        Assertions.assertEquals("create", requests.get(0).getName());
    }

    @Test
    public void executeWithAuthAndException() {
        String[] args = { "src/test/resources/auth-request.spec.http" };

        requestCatcher.add(correlationId, "request-with-exception", "authorization");
        requestCatcher.add(correlationId, "request-with-exception-throw", new CommandException("authorization"));

        Assertions.assertThrows(CommandException.class,
            () -> new RequestCommand(args).execute());
    }

    @Test
    public void executeNoRequests() {
        String[] args = { "src/test/resources/empty.spec.http" };

        Assertions.assertThrows(CommandException.class,
            () -> new RequestCommand(args).execute());
    }

    @Test
    public void executeDefinitionNotFound() {
        String[] args = { "-n", "post", "src/test/resources/basic-functions.spec.http" };

        Assertions.assertThrows(CommandException.class,
            () -> new RequestCommand(args).execute());
    }

    @Test
    public void executeConditionsAccepted() {
        String[] args = { "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Optional<AssertionResult> conditionsResult = Optional.of(new AssertionResult(1));
        conditionsResult.get().setPassed(true);

        requestCatcher.add(correlationId, "conditions-result", conditionsResult);

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        boolean executorExecuted = requestCatcher.getLast(correlationId, "executor-executed");
        boolean processorExecuted = requestCatcher.getLast(correlationId, "processor-executed");

        Assertions.assertTrue(executorExecuted);
        Assertions.assertTrue(processorExecuted);
    }

    @Test
    public void executeConditionsFailed() {
        String[] args = { "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Optional<AssertionResult> conditionsResult = Optional.of(new AssertionResult(1));
        conditionsResult.get().setPassed(false);

        requestCatcher.add(correlationId, "conditions-result", conditionsResult);

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        boolean executorExecuted = requestCatcher.getLast(correlationId, "executor-executed");
        boolean processorExecuted = requestCatcher.getLast(correlationId, "processor-executed");

        Assertions.assertFalse(executorExecuted);
        Assertions.assertFalse(processorExecuted);
    }

    @Test
    public void executeOverridedRequest() {
        String[] args = { "-or", "src/test/resources/basic-functions.body.json",
            "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        HTTPRequestEntry request = requestCatcher.getLast(correlationId, "request");
        Map<String, String> body = jsonToObject(request.getBodyContent(), new TypeToken<Map<String, String>>() {});
        
        Assertions.assertEquals("187b03f1-55d5-4d6b-954d-51cf5f20c952", body.get("access"));
    }

    @Test
    public void executeAssertionsAccepted() {
        String[] args = { "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Optional<AssertionResult> assertionsResult = Optional.of(new AssertionResult(1));
        assertionsResult.get().setPassed(true);

        requestCatcher.add(correlationId, "assertions-result", assertionsResult);

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());
    }

    @Test
    public void executeAssertionsFailed() {
        String[] args = { "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Optional<AssertionResult> assertionsResult = Optional.of(new AssertionResult(1));
        assertionsResult.get().setPassed(false);

        requestCatcher.add(correlationId, "assertions-result", assertionsResult);

        Assertions.assertThrows(InvalidAssertionsFoundException.class,
            () -> new RequestCommand(args).execute());
    }

    @Test
    public void executeAndOpenEditor() {
        String[] args = { "-oe", "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Map<String, String> properties = new HashMap<>();
        properties.put("openEditorCommand", "echo test");
        Settings.mergeProperties("default", properties);

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        Settings settings = requestCatcher.getLast(correlationId, "settings");

        Assertions.assertTrue(settings.isOpenOutputInEditor());
        Assertions.assertEquals("echo test", settings.getOpenEditorCommand());
    }

    @Test
    public void executeAndOpenEditorEmptyCommand() {
        String[] args = { "-oe", "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Map<String, String> properties = new HashMap<>();
        properties.put("openEditorCommand", "");
        Settings.mergeProperties("default", properties);

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        Settings settings = requestCatcher.getLast(correlationId, "settings");

        Assertions.assertTrue(settings.isOpenOutputInEditor());
        Assertions.assertTrue(settings.getOpenEditorCommand().isEmpty());
    }

    @Test
    public void executeAndOpenEditorErrorCommand() {
        String[] args = { "-oe", "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Map<String, String> properties = new HashMap<>();
        properties.put("openEditorCommand", "cmd-not-found arg1");
        Settings.mergeProperties("default", properties);

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        Settings settings = requestCatcher.getLast(correlationId, "settings");

        Assertions.assertTrue(settings.isOpenOutputInEditor());
        Assertions.assertEquals("cmd-not-found arg1", settings.getOpenEditorCommand());
    }

    @Test
    public void executePrintOnly() {
        String[] args = { "-p", "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        Settings settings = requestCatcher.getLast(correlationId, "settings");

        Assertions.assertEquals(PRINT_ONLY, settings.getRequestBehaviour());
    }

    @Test
    public void executeCurlOnly() {
        String[] args = { "-c", "-n", "create", "src/test/resources/basic-functions.spec.http" };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        Settings settings = requestCatcher.getLast(correlationId, "settings");

        Assertions.assertEquals(CURL_ONLY, settings.getRequestBehaviour());
    }

    @Test
    public void executeFlow() {
        String[] args = { "-n", "basicWithAuthorization", "src/test/resources/flow.spec.http" };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, "request");

        Assertions.assertEquals(5, requests.size());
    }

    @Test
    public void executeFlowRecursive() {
        String[] args = { "-n", "invalidRecursiveFlow", "src/test/resources/flow.spec.http" };

        Assertions.assertThrows(RecursiveCommandException.class,
            () -> new RequestCommand(args).execute());
    }

    @Test
    public void executeFlowNoSteps() {
        String[] args = { "-n", "noSteps", "src/test/resources/flow.spec.http" };

        Assertions.assertThrows(CommandException.class,
            () -> new RequestCommand(args).execute());
    }

    @Test
    public void executeFlowStepError() {
        String[] args = { "-n", "errorOnStep", "src/test/resources/flow.spec.http" };

        Assertions.assertThrows(CommandException.class,
            () -> new RequestCommand(args).execute());
    }

    @Test
    public void executeFlowLevels() {
        String[] args = { "-t", "2", "-n", "levels", "src/test/resources/flow.spec.http" };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, "request");

        Assertions.assertEquals(8, requests.size());
    }

    @Test
    public void writeOnEmptyHistory() throws IOException {
        Settings settings = new Settings();
        UUID request = UUID.randomUUID();
        Path sourcePath = Paths.get("src/test/resources/basic-functions.spec.http");
        Path targetPath = settings.getExecutionPath().resolve(request + ".http");

        Files.copy(sourcePath, targetPath);

        String[] args = { "-n", "create", targetPath.toString() };

        Assertions.assertDoesNotThrow(() -> new RequestCommand(args).execute());

        Path historyPath = settings.getHistoryPath()
            .resolve("executions/")
            .resolve(request + "_http/create/" + settings.getTimestamp().toLocalDate() + "/")
            .resolve(settings.getTimestamp().toLocalDate() + ".history.json");

        Assertions.assertTrue(historyPath.toFile().exists());
        Assertions.assertTrue(historyPath.toFile().delete());
        Assertions.assertTrue(targetPath.toFile().delete());
    }
}
