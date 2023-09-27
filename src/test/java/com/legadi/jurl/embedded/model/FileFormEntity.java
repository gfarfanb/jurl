package com.legadi.jurl.embedded.model;

import java.time.LocalDateTime;
import java.util.UUID;

import com.google.gson.annotations.JsonAdapter;
import com.legadi.jurl.embedded.model.adapter.LocalDateTimeDeserializer;

public class FileFormEntity {

    private UUID identifier;
    @JsonAdapter(LocalDateTimeDeserializer.class)
    private LocalDateTime timestamp;
    private String filename;
    private String type;
    private String field;

    public UUID getIdentifier() {
        return identifier;
    }

    public void setIdentifier(UUID identifier) {
        this.identifier = identifier;
    }

    public LocalDateTime getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(LocalDateTime timestamp) {
        this.timestamp = timestamp;
    }

    public String getFilename() {
        return filename;
    }

    public void setFilename(String filename) {
        this.filename = filename;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
    }

}
