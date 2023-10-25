package com.legadi.jurl.embedded.controller;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

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

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.model.FileFormEntity;
import com.legadi.jurl.embedded.util.RequestCatcher;

@RestController
@RequestMapping("/file")
public class FileFormController {

    @Autowired
    private RequestCatcher requestCatcher;

    @PostMapping(consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<String> upload(
            @RequestParam Map<String,String> requestParams,
            @RequestPart MultipartFile file) throws IOException {
        Settings settings = new Settings();
        Path temporalFile = settings.getExecutionPath().resolve(file.getOriginalFilename());
        UUID id = UUID.randomUUID();

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

        requestCatcher.add(id, "file-body", entity);

        HttpHeaders headers = new HttpHeaders();
        headers.set("Resource-ID", id.toString());

        return ResponseEntity.status(HttpStatus.CREATED)
            .headers(headers)
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
