package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.WriterUtils.cleanDirectory;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import com.legadi.cli.jurl.common.DeleteFileVisitor;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "--remove", alias = "-rm")
public class CleanOutputOption extends Option {

    public static final String ALL_FILES = "all";
    public static final String OUTPUT_FILES = "out";

    @Override
    public String[] getArgs() {
        return new String[] { "until-date" };
    }

    @Override
    public String getDescription() {
        return "Remove all the execution outputs and history files from the specified date (inclusive): 'yyyy-MM-dd'. If '" + ALL_FILES + "' is received, all output files will be deleted. If '" + OUTPUT_FILES + "' is received, only configuration output files will be deleted.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        if(OUTPUT_FILES.equalsIgnoreCase(args[0])) {
            cleanDirectory(settings.getConfigOutputPath(), new DeleteFileVisitor(null));
            return false;
        }

        LocalDate untilDateInclusive = null;
        if(!ALL_FILES.equalsIgnoreCase(args[0])) {
            DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE;
            untilDateInclusive = LocalDate.from(formatter.parse(args[0]));
        }

        cleanDirectory(settings.getExecutionPath(), new DeleteFileVisitor(untilDateInclusive));
        cleanDirectory(settings.getHistoryPath(), new DeleteFileVisitor(untilDateInclusive));
        cleanDirectory(settings.getConfigOutputPath(), new DeleteFileVisitor(untilDateInclusive));
        return false;
    }
}
