package com.legadi.cli.jurl.embedded.config;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableAutoConfiguration
@ComponentScan(basePackages = "com.legadi.cli.jurl.embedded.controller")
public class EmbeddedConfig {

}
