package com.legadi.jurl.executor.mixer;

import static com.legadi.jurl.executor.mixer.BodyMixerRegistry.findByBodyType;
import static com.legadi.jurl.executor.mixer.BodyMixerRegistry.registerMixer;

import java.nio.file.Path;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;

public class BodyMixerRegistryTest {

    @Test
    public void registerMixerCustom() {
        registerMixer(TestMixer.class.getName());

        Assertions.assertDoesNotThrow(() -> (TestMixer) findByBodyType("test"));
    }

    @Test
    public void notFound() {
        Assertions.assertThrows(CommandException.class,
            () -> findByBodyType("not-found"));
    }

    public static class TestMixer implements BodyMixer {

        @Override
        public String type() {
            return "test";
        }

        @Override
        public Path apply(Settings settings, MixerEntry entry) {
            return null;
        }
    }
}
