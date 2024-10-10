package com.legadi.cli.jurl.modifiers;

import java.io.File;
import java.nio.file.Files;
import java.util.function.Function;

public class ReadFileModifier implements ValueModifier {

    @Override
    public String name() {
        return "read-file";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String apply(Function<String, String> getter, String[] args, String value) throws Exception {
        try {
            File output = new File(value);
            if(output.exists()) {
                return new String(Files.readAllBytes(output.toPath()));
            } else {
                return value;
            }
        } catch(Exception ex) {
            return value;
        }
    }
    
}
