package com.legadi.jurl.embedded.wrong;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Writer;

public class UnwritableWriter extends BufferedWriter {

    public UnwritableWriter(Writer out) {
        super(out);
    }

    @Override
    public void write(String str) throws IOException {
        throw new IOException("Trying write(" + str + ")");
    }
}
