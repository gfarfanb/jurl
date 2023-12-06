package com.legadi.jurl.executor.mixer;

import java.nio.file.Path;
import java.nio.file.Paths;

import com.legadi.jurl.common.Evaluable;
import com.legadi.jurl.common.Settings;

public interface BodyMixer extends Evaluable {

    @Override
    default boolean accepts(String bodyType) {
        return type().equalsIgnoreCase(bodyType);
    }

    String type();

    Path apply(Settings settings, MixerEntry entry);

    public static class MixerEntry {

        private String requestPath;
        private String requestName;
        private Path bodyFilePath;
        private String bodyContent;

        public String getRequestPath() {
            return requestPath;
        }

        public MixerEntry setRequestPath(String requestPath) {
            this.requestPath = requestPath;
            return this;
        }

        public String getRequestName() {
            return requestName;
        }

        public MixerEntry setRequestName(String requestName) {
            this.requestName = requestName;
            return this;
        }

        public Path getBodyFilePath() {
            return bodyFilePath;
        }

        public MixerEntry setBodyFilePath(String bodyFilePath) {
            this.bodyFilePath = Paths.get(bodyFilePath);
            return this;
        }

        public String getBodyContent() {
            return bodyContent;
        }

        public MixerEntry setBodyContent(String bodyContent) {
            this.bodyContent = bodyContent;
            return this;
        }

    }
}
