package com.legadi.jurl.model;

public enum AuthorizationType {

    EMPTY, BASIC, TOKEN;

    public static AuthorizationType valueOfOrDefault(String type) {
        for(AuthorizationType authType : values()) {
            if(authType.name().equalsIgnoreCase(type)) {
                return authType;
            }
        }
        return AuthorizationType.EMPTY;
    }
}
