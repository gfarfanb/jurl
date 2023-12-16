package com.legadi.jurl.embedded.wrong;

import java.io.IOException;
import java.io.InputStream;

public class UnreadableInputStream extends InputStream {

    @Override
    public int read() throws IOException {
        throw new IOException("Trying read()");
    }
}
