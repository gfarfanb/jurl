package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.model.ExecutionStatus.FAILED;
import static com.legadi.cli.jurl.model.ExecutionStatus.PARTIALLY;
import static com.legadi.cli.jurl.model.ExecutionStatus.SKIPPED;
import static com.legadi.cli.jurl.model.ExecutionStatus.SUCCESSFUL;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class ExecutionStatsTest {

    @Test
    public void countValidation() {
        ExecutionStats stats = new ExecutionStats(10);

        stats.count(FAILED);
        stats.count(PARTIALLY);
        stats.count(PARTIALLY);
        stats.count(SKIPPED);
        stats.count(SKIPPED);
        stats.count(SKIPPED);
        stats.count(SUCCESSFUL);
        stats.count(SUCCESSFUL);
        stats.count(SUCCESSFUL);
        stats.count(SUCCESSFUL);

        Assertions.assertEquals(10, stats.getExecutions());
        Assertions.assertEquals(4, stats.getCount(SUCCESSFUL));
        Assertions.assertEquals(3, stats.getCount(SKIPPED));
        Assertions.assertEquals(2, stats.getCount(PARTIALLY));
        Assertions.assertEquals(1, stats.getCount(FAILED));
    }

    @Test
    public void computeExpected() {
        ExecutionStats statsFailed = new ExecutionStats(2);

        statsFailed.count(FAILED);
        statsFailed.count(FAILED);

        Assertions.assertEquals(FAILED, statsFailed.computeStatus());

        ExecutionStats statsPartially = new ExecutionStats(1);

        statsPartially.count(PARTIALLY);

        Assertions.assertEquals(PARTIALLY, statsPartially.computeStatus());

        ExecutionStats statsSkipped = new ExecutionStats(3);

        statsSkipped.count(SKIPPED);
        statsSkipped.count(SKIPPED);
        statsSkipped.count(SKIPPED);

        Assertions.assertEquals(SKIPPED, statsSkipped.computeStatus());

        ExecutionStats statsSuccessful = new ExecutionStats(5);

        statsSuccessful.count(SUCCESSFUL);
        statsSuccessful.count(SUCCESSFUL);
        statsSuccessful.count(SUCCESSFUL);
        statsSuccessful.count(SUCCESSFUL);
        statsSuccessful.count(SUCCESSFUL);

        Assertions.assertEquals(SUCCESSFUL, statsSuccessful.computeStatus());
    }

    @Test
    public void computePartially() {
        ExecutionStats stats = new ExecutionStats(4);

        stats.count(FAILED);
        stats.count(PARTIALLY);
        stats.count(SKIPPED);
        stats.count(SUCCESSFUL);

        Assertions.assertEquals(PARTIALLY, stats.computeStatus());
    }

    @Test
    public void toStringValidation() {
        ExecutionStats stats = new ExecutionStats(10);

        stats.count(FAILED);
        stats.count(SKIPPED);
        stats.count(SKIPPED);
        stats.count(SKIPPED);
        stats.count(SUCCESSFUL);
        stats.count(SUCCESSFUL);
        stats.count(SUCCESSFUL);
        stats.count(SUCCESSFUL);

        String printableStats = stats.toString();

        Assertions.assertTrue(printableStats.contains(FAILED + "=" + 1));
        Assertions.assertTrue(printableStats.contains(SKIPPED + "=" + 3));
        Assertions.assertTrue(printableStats.contains(SUCCESSFUL + "=" + 4));
        Assertions.assertFalse(printableStats.contains(PARTIALLY + "="));
    }
}
