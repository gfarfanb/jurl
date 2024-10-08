package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.WriterUtils.cleanDirectory;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import com.legadi.cli.jurl.common.DeleteFileVisitor;
import com.legadi.cli.jurl.common.Settings;

public class CleanOutputOption extends Option {

    public static final String ALL_FILES = "all";

    @Override
    public String name() {
        return "--remove";
    }

    @Override
    public String alias() {
        return "-rm";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "until-date" };
    }

    @Override
    public String getDescription() {
        return "Remove all the execution outputs and history files from the specified date (inclusive): 'yyyy-MM-dd'. If 'all' is received in the argument all output files will be deleted.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        LocalDate untilDateInclusive = null;
        if(!ALL_FILES.equalsIgnoreCase(args[0])) {
            DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE;
            untilDateInclusive = LocalDate.from(formatter.parse(args[0]));
        }
        cleanDirectory(settings.getExecutionPath(), new DeleteFileVisitor(untilDateInclusive));
        cleanDirectory(settings.getHistoryPath(), new DeleteFileVisitor(untilDateInclusive));
        return false;
    }
}
