package com.legadi.cli.jurl.embedded.controller;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.embedded.util.ObjectName.BODY;
import static com.legadi.cli.jurl.embedded.util.RequestCatcherManager.getCatcher;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.UUID;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.model.FileFormEntity;
import com.legadi.cli.jurl.embedded.util.RequestCatcher;

@RestController
@RequestMapping("/file")
public class FileFormController {

    @PostMapping(consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<String> upload(
            @RequestParam Map<String,String> requestParams,
            @RequestPart MultipartFile file,
            @RequestHeader Map<String, String> requestHeaders) throws IOException {
        RequestCatcher requestCatcher = getCatcher(requestHeaders.get("request-catcher"));

        Settings settings = new Settings();
        Path temporalFile = settings.getExecutionPath().resolve(file.getOriginalFilename());
        UUID correlationId = UUID.randomUUID();

        try(InputStream fileInputStream = file.getInputStream();
                OutputStream outputStream = Files.newOutputStream(temporalFile)) {

            byte[] buffer = new byte[8 * 1024];
            int bytesRead;
            while ((bytesRead = fileInputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }
        }

        FileFormEntity entity = new FileFormEntity();
        if(isNotBlank(requestParams.get("identifier"))) {
            entity.setIdentifier(UUID.fromString(requestParams.get("identifier")));
        }
        if(isNotBlank(requestParams.get("timestamp"))) {
            entity.setTimestamp(LocalDateTime.from(
                DateTimeFormatter.ISO_LOCAL_DATE_TIME.parse(requestParams.get("timestamp"))
            ));
        }
        entity.setFilename(file.getOriginalFilename());
        entity.setType(file.getContentType());
        entity.setField(file.getName());

        requestCatcher.add(correlationId, BODY, entity);

        HttpHeaders headers = new HttpHeaders();
        headers.set("Resource-ID", correlationId.toString());

        return ResponseEntity.status(HttpStatus.CREATED)
            .headers(headers)
            .body("Created");
    }

    @PostMapping(path = "/set", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<String> uploadFiles(
            @RequestPart MultipartFile[] files) throws IOException {
        Settings settings = new Settings();

        for(MultipartFile file : files) {
            Path temporalFile = settings.getExecutionPath().resolve(file.getOriginalFilename());

            try(InputStream fileInputStream = file.getInputStream();
                    OutputStream outputStream = Files.newOutputStream(temporalFile)) {

                byte[] buffer = new byte[8 * 1024];
                int bytesRead;
                while ((bytesRead = fileInputStream.read(buffer)) != -1) {
                    outputStream.write(buffer, 0, bytesRead);
                }
            }
        }

        return ResponseEntity.status(HttpStatus.CREATED)
            .body("Created");
    }

    @GetMapping
    public StreamingResponseBody download(
            @RequestParam String file,
            @RequestParam(required = false) String name,
            HttpServletResponse response) throws IOException {
        Path filePath = Paths.get(file);
        String mimeType = Files.probeContentType(filePath);

        response.setContentType(mimeType);

        if(isNotBlank(name)) {
            response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + name);
        } else {
            response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment");
        }

        return outputStream -> {
            try(InputStream fileInputStream = Files.newInputStream(filePath)) {

                byte[] buffer = new byte[8 * 1024];
                int bytesRead;
                while ((bytesRead = fileInputStream.read(buffer)) != -1) {
                    outputStream.write(buffer, 0, bytesRead);
                }
            }
        };
    }
}
