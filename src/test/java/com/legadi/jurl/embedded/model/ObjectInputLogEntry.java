package com.legadi.jurl.embedded.model;

import java.time.LocalDateTime;

import com.google.gson.annotations.JsonAdapter;
import com.legadi.jurl.embedded.model.adapter.LocalDateTimeDeserializer;

public class ObjectInputLogEntry {

    @JsonAdapter(LocalDateTimeDeserializer.class)
    private LocalDateTime timestamp;
    private boolean completed;

    public LocalDateTime getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(LocalDateTime timestamp) {
        this.timestamp = timestamp;
    }

    public boolean isCompleted() {
        return completed;
    }

    public void setCompleted(boolean completed) {
        this.completed = completed;
    }
}
