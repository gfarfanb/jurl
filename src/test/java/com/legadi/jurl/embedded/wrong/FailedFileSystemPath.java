package com.legadi.jurl.embedded.wrong;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.FileSystem;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.WatchEvent.Kind;
import java.nio.file.WatchEvent.Modifier;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.Iterator;

public class FailedFileSystemPath implements Path {

    protected final Path path;

    public FailedFileSystemPath(Path path) {
        this.path = path;
    }

    @Override
    public FileSystem getFileSystem() {
        throw new IllegalStateException("Trying getFileSystem()");
    }

    @Override
    public boolean isAbsolute() {
        return path.isAbsolute();
    }

    @Override
    public Path getRoot() {
        return path.getRoot();
    }

    @Override
    public Path getFileName() {
        return path.getFileName();
    }

    @Override
    public Path getParent() {
        return path.getParent();
    }

    @Override
    public int getNameCount() {
        return path.getNameCount();
    }

    @Override
    public Path getName(int index) {
        return path.getName(index);
    }

    @Override
    public Path subpath(int beginIndex, int endIndex) {
        return path.subpath(beginIndex, endIndex);
    }

    @Override
    public boolean startsWith(Path other) {
        return path.startsWith(other);
    }

    @Override
    public boolean startsWith(String other) {
        return path.startsWith(other);
    }

    @Override
    public boolean endsWith(Path other) {
        return path.endsWith(other);
    }

    @Override
    public boolean endsWith(String other) {
        return path.endsWith(other);
    }

    @Override
    public Path normalize() {
        return path.normalize();
    }

    @Override
    public Path resolve(Path other) {
        return path.resolve(other);
    }

    @Override
    public Path resolve(String other) {
        return path.resolve(other);
    }

    @Override
    public Path resolveSibling(Path other) {
        return path.resolveSibling(other);
    }

    @Override
    public Path resolveSibling(String other) {
        return path.resolveSibling(other);
    }

    @Override
    public Path relativize(Path other) {
        return path.relativize(other);
    }

    @Override
    public URI toUri() {
        return path.toUri();
    }

    @Override
    public Path toAbsolutePath() {
        return path.toAbsolutePath();
    }

    @Override
    public Path toRealPath(LinkOption... options) throws IOException {
        return path.toRealPath(options);
    }

    @Override
    public File toFile() {
        return path.toFile();
    }

    @Override
    public WatchKey register(WatchService watcher, Kind<?>[] events, Modifier... modifiers) throws IOException {
        return path.register(watcher, events, modifiers);
    }

    @Override
    public WatchKey register(WatchService watcher, Kind<?>... events) throws IOException {
        return path.register(watcher, events);
    }

    @Override
    public Iterator<Path> iterator() {
        return path.iterator();
    }

    @Override
    public int compareTo(Path other) {
        return path.compareTo(other);
    }
}
