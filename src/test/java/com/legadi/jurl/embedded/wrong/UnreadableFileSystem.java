package com.legadi.jurl.embedded.wrong;

import java.io.IOException;
import java.nio.file.FileStore;
import java.nio.file.FileSystem;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.WatchService;
import java.nio.file.attribute.UserPrincipalLookupService;
import java.nio.file.spi.FileSystemProvider;
import java.util.Set;

public class UnreadableFileSystem extends FileSystem {

    protected final FileSystem fileSystem;

    public UnreadableFileSystem(FileSystem fileSystem) {
        this.fileSystem = fileSystem;
    }

    @Override
    public FileSystemProvider provider() {
        return new UnreadableFileSystemProvider(fileSystem.provider());
    }

    @Override
    public void close() throws IOException {
        fileSystem.close();
    }

    @Override
    public boolean isOpen() {
        return fileSystem.isOpen();
    }

    @Override
    public boolean isReadOnly() {
        return fileSystem.isReadOnly();
    }

    @Override
    public String getSeparator() {
        return fileSystem.getSeparator();
    }

    @Override
    public Iterable<Path> getRootDirectories() {
        return fileSystem.getRootDirectories();
    }

    @Override
    public Iterable<FileStore> getFileStores() {
        return fileSystem.getFileStores();
    }

    @Override
    public Set<String> supportedFileAttributeViews() {
        return fileSystem.supportedFileAttributeViews();
    }

    @Override
    public Path getPath(String first, String... more) {
        return fileSystem.getPath(first, more);
    }

    @Override
    public PathMatcher getPathMatcher(String syntaxAndPattern) {
        return fileSystem.getPathMatcher(syntaxAndPattern);
    }

    @Override
    public UserPrincipalLookupService getUserPrincipalLookupService() {
        return fileSystem.getUserPrincipalLookupService();
    }

    @Override
    public WatchService newWatchService() throws IOException {
        return fileSystem.newWatchService();
    }
}
