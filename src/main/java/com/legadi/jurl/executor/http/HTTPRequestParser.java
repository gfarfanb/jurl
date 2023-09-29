package com.legadi.jurl.executor.http;

import static com.legadi.jurl.common.CommonUtils.strip;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.Closeable;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.legadi.jurl.common.OutputPathBuilder;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.common.StringExpander;
import com.legadi.jurl.model.AssertionEntry;
import com.legadi.jurl.model.AssertionType;
import com.legadi.jurl.model.http.HTTPRequestEntry;

public class HTTPRequestParser {

    private static final String HEADER_PATTERN = "^([\\w-]+): (.*)";
    private static final String QUERY_PARAM_PATTERN = "^([\\w.-]+)=(.*)";
    private static final String OUTPUT_MAPPING_PATTERN = "^([\\w:.-_]+) <- (.*)";
    private static final String ASSERTION_PATTERN = "^([\\w._]+)! (.*)";
    private static final String COMMENT_PATTERN = "^#(.*)";

    private final StringExpander stringExpander;
    private final Pattern headerPattern;
    private final Pattern queryParamPattern;
    private final Pattern outputMappingPattern;
    private final Pattern assertionPattern;
    private final Pattern commentPattern;

    public HTTPRequestParser(Settings settings) {
        this.stringExpander = new StringExpander(settings);
        this.headerPattern = Pattern.compile(HEADER_PATTERN);
        this.queryParamPattern = Pattern.compile(QUERY_PARAM_PATTERN);
        this.outputMappingPattern = Pattern.compile(OUTPUT_MAPPING_PATTERN);
        this.assertionPattern = Pattern.compile(ASSERTION_PATTERN);
        this.commentPattern = Pattern.compile(COMMENT_PATTERN);
    }

    public HTTPRequestEntry parseRequest(String requestPath, String requestName, String filename) {
        HTTPRequestEntry request = new HTTPRequestEntry();

        request.setHeaders(new HashMap<>());
        request.setQueryParams(new HashMap<>());
        request.setOutputMappings(new HashMap<>());
        request.setAssertions(new LinkedList<>());

        try (BufferedReader br = new BufferedReader(new FileReader(filename));
                BodyWriter writer = new BodyWriter()) {
            String line;

            while ((line = br.readLine()) != null) {
                line = stringExpander.replaceAllInContent(line);

                if(addHeader(request, line)) {
                    continue;
                }

                if(addQueryParam(request, line)) {
                    continue;
                }

                if(addOutputMapping(request, line)) {
                    continue;
                }

                if(addAssertion(request, line)) {
                    continue;
                }

                if(isComment(line)) {
                    continue;
                }

                if(!writer.hasWriter()) {
                    OutputPathBuilder pathBuilder = new OutputPathBuilder(stringExpander.getSettings())
                        .setRequestPath(requestPath)
                        .setRequestName(requestName)
                        .setExtension("body");
                    Path bodyPath = pathBuilder.buildCommandPath();

                    writer.setWriter(bodyPath.toFile());
                    request.setBodyTemporalPath(bodyPath.toString());
                }

                writer.write(line);
            }
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to process override request file: " + filename, ex);
        }

        return request;
    }

    private boolean addHeader(HTTPRequestEntry request, String line) {
        Matcher matcher = headerPattern.matcher(line);

        if(matcher.find()) {
            String header = matcher.group(0).trim();
            matcher.find();
            String value = matcher.group(0).trim();
            request.getHeaders().put(header, value);
            return true;
        } else {
            return false;
        }
    }

    private boolean addQueryParam(HTTPRequestEntry request, String line) {
        Matcher matcher = queryParamPattern.matcher(line);

        if(matcher.find()) {
            String queryParam = matcher.group(0).trim();
            matcher.find();
            String value = matcher.group(0).trim();
            request.getQueryParams().put(queryParam, value);
            return true;
        } else {
            return false;
        }
    }

    private boolean addOutputMapping(HTTPRequestEntry request, String line) {
        Matcher matcher = outputMappingPattern.matcher(line);

        if(matcher.find()) {
            String outputMapping = matcher.group(0).trim();
            matcher.find();
            String mapping = matcher.group(0).trim();
            request.getOutputMappings().put(outputMapping, mapping);
            return true;
        } else {
            return false;
        }
    }

    private boolean addAssertion(HTTPRequestEntry request, String line) {
        Matcher matcher = assertionPattern.matcher(line);

        if(matcher.find()) {
            String assertion = matcher.group(0).trim();
            matcher.find();
            String[] argsRaw = matcher.group(0).trim().split(" ");
            List<String> args = new ArrayList<>();
            StringBuilder argBuilder = new StringBuilder();
            boolean isQuoted = false;

            for(String arg : argsRaw) {
                if(isQuoted && arg.endsWith("\"")) {
                    isQuoted = false;
                    
                    argBuilder.append(arg);
                    args.add(strip(argBuilder.toString(), "\""));

                    argBuilder = new StringBuilder();
                } else if(isQuoted || arg.startsWith("\"")) {
                    isQuoted = true;

                    argBuilder.append(arg).append(' ');
                } else {
                    args.add(arg);
                }
            }

            AssertionEntry entry = new AssertionEntry();
            AssertionType type = AssertionType.valueOfName(assertion);

            if(type != null) {
                entry.setType(type);
            } else {
                entry.setAssertionClass(assertion);
            }

            entry.setArgs(args.toArray(new String[args.size()]));
            request.getAssertions().add(entry);
            return true;
        } else {
            return false;
        }
    }

    private boolean isComment(String line) {
        Matcher matcher = commentPattern.matcher(line);
        return matcher.matches();
    }

    public static class BodyWriter implements Closeable {

        private BufferedWriter writer;

        public boolean hasWriter() {
            return writer != null;
        }

        public void setWriter(File bodyFile) throws IOException {
            writer = new BufferedWriter(new FileWriter(bodyFile));
        }

        public void write(String line) throws IOException {
            writer.write(line);
            writer.newLine();
        }

        @Override
        public void close() throws IOException {
            if(writer != null) {
                writer.close();
            }
        }
    }
}
