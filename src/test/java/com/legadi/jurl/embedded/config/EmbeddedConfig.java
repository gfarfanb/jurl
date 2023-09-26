package com.legadi.jurl.embedded.config;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import com.legadi.jurl.embedded.util.RequestCatcher;

@Configuration
@EnableAutoConfiguration
@ComponentScan(basePackages = "com.legadi.jurl.embedded.controller")
public class EmbeddedConfig {

    @Bean
    public RequestCatcher requestCatcher() {
        return new RequestCatcher();
    }
}
