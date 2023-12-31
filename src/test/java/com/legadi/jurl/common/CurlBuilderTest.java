package com.legadi.jurl.common;

import java.io.IOException;
import java.net.URL;
import java.nio.file.Paths;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.embedded.EmbeddedAPITest;

public class CurlBuilderTest extends EmbeddedAPITest {

    @Test
    public void buildPostBinary() throws IOException {
        CurlBuilder curlBuilder = new CurlBuilder()
            .setUrl(new URL("http://localhost:" + port + "/basic/body"))
            .setMethod("POST")
            .setDataBinary(Paths.get("src/test/resources/basic-functions.body.json"))
            .addHeader("Content-Type", "application/json");
        String expected = "curl -X POST"
            + " -H \"Content-Type: application/json\""
            + " --data-binary \"@src/test/resources/basic-functions.body.json\""
            + " \"http://localhost:" + port + "/basic/body\"";

        Assertions.assertEquals(expected, curlBuilder.build());
    }

    @Test
    public void buildWithoutMethod() throws IOException {
        CurlBuilder curlBuilder = new CurlBuilder()
            .setUrl(new URL("http://localhost:" + port + "/basic/body"));
        String expected = "curl \"http://localhost:" + port + "/basic/body\"";

        Assertions.assertEquals(expected, curlBuilder.build());
    }

    @Test
    public void buildWithoutURL() throws IOException {
        CurlBuilder curlBuilder = new CurlBuilder();
        String expected = "curl";

        Assertions.assertEquals(expected, curlBuilder.build());
    }

    @Test
    public void buildPostFile() throws IOException {
        CurlBuilder curlBuilder = new CurlBuilder()
            .setUrl(new URL("http://localhost:" + port + "/file"))
            .setMethod("POST")
            .addForm("timestamp", "2023-10-13T03:50:54.792")
            .setFile("file", "src/test/resources/file.csv", "uploaded.csv", "text/csv")
            .addHeader("Content-Type", "multipart/form-data");
        String expected = "curl -X POST"
            + " -H \"Content-Type: multipart/form-data\""
            + " -F \"timestamp=2023-10-13T03:50:54.792\""
            + " -F \"file=@src/test/resources/file.csv;filename=uploaded.csv;type=text/csv\""
            + " \"http://localhost:" + port + "/file\"";

        Assertions.assertEquals(expected, curlBuilder.build());
    }

    @Test
    public void buildPostFileWithoutName() throws IOException {
        CurlBuilder curlBuilder = new CurlBuilder()
            .setUrl(new URL("http://localhost:" + port + "/file"))
            .setMethod("POST")
            .setFile("file", "src/test/resources/file.csv", null, null)
            .addHeader("Content-Type", "multipart/form-data");
        String expected = "curl -X POST"
            + " -H \"Content-Type: multipart/form-data\""
            + " -F \"file=@src/test/resources/file.csv\""
            + " \"http://localhost:" + port + "/file\"";

        Assertions.assertEquals(expected, curlBuilder.build());
    }
}
