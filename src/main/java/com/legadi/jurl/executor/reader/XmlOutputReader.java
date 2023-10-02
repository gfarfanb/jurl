package com.legadi.jurl.executor.reader;

import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
// import java.util.Deque;
// import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

public class XmlOutputReader implements OutputReader {

    @Override
    public boolean accepts(String contentType) {
        return accepts(contentType, "application/xml", "text/xml", "application/atom+xml");
    }

    @Override
    public boolean isPrintable() {
        return true;
    }

    @Override
    public Map<String, String> apply(Path sourcePath, Path outputPath, Set<String> params, String paramPrefix) {
        try(Reader fileReader = Files.newBufferedReader(sourcePath)) {
            XMLInputFactory xmlInputFactory = XMLInputFactory.newInstance();
            XMLStreamReader xmlStreamReader  = xmlInputFactory.createXMLStreamReader(fileReader);
            // Deque<String> element = new LinkedList<>();
            boolean complete = false;

            while(!complete && xmlStreamReader.hasNext()){
                int xmlEvent = xmlStreamReader.next();

                switch(xmlEvent) {
                    case XMLStreamConstants.START_ELEMENT:
                        // String elementName = xmlStreamReader.getLocalName();
                        int attributesLength = xmlStreamReader.getAttributeCount();

                        for(int i=0; i< attributesLength; i++) {
                            // String attributeName = xmlStreamReader.getAttributeLocalName(i);
                            // String attributeValue = xmlStreamReader.getAttributeValue(i);
                        }
                        break;
                    case XMLStreamConstants.CHARACTERS:
                        // String elementValue = xmlStreamReader.getElementText();
                        break;
                    case XMLStreamConstants.END_ELEMENT:
                        break;
                    case XMLStreamConstants.END_DOCUMENT:
                        complete = true;
                        break;
                }
            }
        } catch(XMLStreamException | IOException ex) {
            throw new IllegalStateException("Unable to read XML file: " + sourcePath, ex);
        }

        throw new UnsupportedOperationException("Read XML logic is not completed");
    }
}
