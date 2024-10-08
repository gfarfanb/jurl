package com.legadi.cli.jurl.embedded.model;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

import com.google.gson.annotations.JsonAdapter;
import com.legadi.cli.jurl.embedded.model.adapter.LocalDateTimeDeserializer;

public class BasicFunctionsEntity {

    private UUID access;
    private String name;
    private String email;
    private String nickname;
    private BigDecimal amount;
    private boolean active;
    private int coins;
    private String bio;
    private String type;
    @JsonAdapter(LocalDateTimeDeserializer.class)
    private LocalDateTime timestamp;

    public UUID getAccess() {
        return access;
    }

    public void setAccess(UUID access) {
        this.access = access;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getNickname() {
        return nickname;
    }

    public void setNickname(String nickname) {
        this.nickname = nickname;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public boolean isActive() {
        return active;
    }

    public void setActive(boolean active) {
        this.active = active;
    }

    public int getCoins() {
        return coins;
    }

    public void setCoins(int coins) {
        this.coins = coins;
    }

    public String getBio() {
        return bio;
    }

    public void setBio(String bio) {
        this.bio = bio;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public LocalDateTime getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(LocalDateTime timestamp) {
        this.timestamp = timestamp;
    }
}
