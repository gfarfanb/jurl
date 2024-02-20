package com.legadi.jurl.model;

import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

import com.legadi.jurl.options.OptionsReader.OptionEntry;

public class AuthenticationRequest<T extends RequestEntry<? extends MockEntry>> {

    private String authRequestInputPath;
    private String authRequestName;
    private AuthorizationType authType;
    private T authApi;
    private T authRequest;
    private List<OptionEntry> authOptions = new LinkedList<>();

    public String getAuthRequestInputPath() {
        return authRequestInputPath;
    }

    public void setAuthRequestInputPath(String authRequestInputPath) {
        this.authRequestInputPath = authRequestInputPath;
    }

    public String getAuthRequestName() {
        return authRequestName;
    }

    public void setAuthRequestName(String authRequestName) {
        this.authRequestName = authRequestName;
    }

    public AuthorizationType getAuthType() {
        return authType;
    }

    public void setAuthType(AuthorizationType authType) {
        this.authType = authType;
    }

    public T getAuthApi() {
        return authApi;
    }

    public void setAuthApi(T authApi) {
        this.authApi = authApi;
    }

    public T getAuthRequest() {
        return authRequest;
    }

    public void setAuthRequest(T authRequest) {
        this.authRequest = authRequest;
    }

    public List<OptionEntry> getAuthOptions() {
        return authOptions;
    }

    public void setAuthOptions(List<OptionEntry> authOptions) {
        this.authOptions = authOptions;
    }

    @Override
    public int hashCode() {
        return Objects.hash(authRequestInputPath, authRequestName, authType);
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        AuthenticationRequest<T> other = (AuthenticationRequest<T>) obj;
        return Objects.equals(authRequestInputPath, other.authRequestInputPath)
            && Objects.equals(authRequestName, other.authRequestName)
            && Objects.equals(authType, other.authType);
    }
}
