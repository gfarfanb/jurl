package com.legadi.cli.jurl.executor.mixer;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;

import com.legadi.cli.jurl.common.Settings;


public interface BodyMixer {

    Path apply(Settings settings, Map<String, Object> defaults, MixerEntry entry);

    public static class MixerEntry {

        private String requestPath;
        private String requestName;
        private Path bodyBasePath;
        private Path bodyComparePath;

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

        public Path getBodyBasePath() {
            return bodyBasePath;
        }

        public MixerEntry setBodyBasePath(String bodyBasePath) {
            this.bodyBasePath = Paths.get(bodyBasePath);
            return this;
        }

        public Path getBodyComparePath() {
            return bodyComparePath;
        }

        public MixerEntry setBodyComparePath(String bodyComparePath) {
            this.bodyComparePath = Paths.get(bodyComparePath);
            return this;
        }

    }
}
