package com.legadi.cli.jurl.executor.decoder;

import static java.util.logging.Level.FINE;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;

public class GzipOutputDecoder implements OutputDecoder {

    private static final Logger LOGGER = Logger.getLogger(GzipOutputDecoder.class.getName());

    @Override
    public String[] types() {
        return new String[] { "gzip", "x-gzip" };
    }

    @Override
    public Path apply(Path sourcePath) {
        if(sourcePath == null) {
            return null;
        }

        Path outputPath = Paths.get(sourcePath.toString().concat(".out"));

        try (GZIPInputStream gzipInputStream = new GZIPInputStream(new FileInputStream(sourcePath.toFile()));
                FileOutputStream outputStream = new FileOutputStream(outputPath.toFile())) {

            byte[] buffer = new byte[1024];
            int lenght;
            while ((lenght = gzipInputStream.read(buffer)) > 0) {
                outputStream.write(buffer, 0, lenght);
            }

            return outputPath;
        } catch(Exception ex) {
            LOGGER.log(FINE, "Error on decompressing from GZIP - " + ex.getMessage(), ex);
            return sourcePath;
        }
    }
}
