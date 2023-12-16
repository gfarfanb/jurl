package com.legadi.jurl.embedded.wrong;

import java.io.IOException;
import java.io.OutputStream;

public class UnwritableOutputStream extends OutputStream {

    @Override
    public void write(int b) throws IOException {
        throw new IOException("Trying write(" + b + ")");
    }
}
