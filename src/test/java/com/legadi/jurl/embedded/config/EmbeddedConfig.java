package com.legadi.jurl.embedded.config;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableAutoConfiguration
@ComponentScan(basePackages = "com.legadi.jurl.embedded.controller")
public class EmbeddedConfig {

    public EmbeddedConfig() {
        System.setProperty("java.util.logging.SimpleFormatter.format", "%5$s%6$s%n");
        System.setProperty("java.util.logging.ConsoleHandler.level", "INFO");
    }
}
