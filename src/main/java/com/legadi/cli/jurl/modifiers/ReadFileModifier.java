package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.annotations.Evaluable.Operation.STARTS_WITH;

import java.io.File;
import java.nio.file.Files;
import java.util.function.Function;

import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "read_file")
@Evaluable(values = { "read_file" }, op = STARTS_WITH)
public class ReadFileModifier implements ValueModifier {

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String apply(Function<String, String> propertyResolver, String[] args, String value) throws Exception {
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
