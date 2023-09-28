package com.legadi.jurl.common;

import java.util.HashSet;
import java.util.Set;

public class ExecutionLevels {

    private static final String TAB = "    ";

    private Level level;

    public void nextLevel() {
        Level nextLevel = new Level();
        nextLevel.setPreviousLevel(level);
        if(level != null) {
            level.setNextLevel(nextLevel);
        }
        level = nextLevel;
    }

    public boolean wasExecuted(String requestInputPath, String inputName) {
        if(level != null) {
            return !level.registerCommand(requestInputPath, inputName);
        } else {
            return false;
        }
    }

    public String getTrace() {
        Level top = level;

        while(top.previousLevel != null) {
            top = top.previousLevel;
        }

        StringBuilder trace = new StringBuilder();
        String tab = "";

        trace.append(top.getCommands()).append("\n");

        while(top.getNextLevel() != null) {
            tab += TAB;
            trace.append(tab).append("-> ").append(top.getNextLevel().getCommands()).append("\n");
            top = top.getNextLevel();
        }

        return trace.toString();
    }

    public static class Level {

        private Set<String> commands = new HashSet<>();
        private Level previousLevel;
        private Level nextLevel;

        public Set<String> getCommands() {
            return commands;
        }

        public Level getPreviousLevel() {
            return previousLevel;
        }

        public void setPreviousLevel(Level previousLevel) {
            this.previousLevel = previousLevel;
        }

        public Level getNextLevel() {
            return nextLevel;
        }

        public void setNextLevel(Level nextLevel) {
            this.nextLevel = nextLevel;
        }

        public boolean registerCommand(String requestInputPath, String inputName) {
            String command = requestInputPath + "/" + inputName;
            boolean canBeRegistered = validate(previousLevel, command);
            if(canBeRegistered) {
                commands.add(command);
            }
            return canBeRegistered;
        }

        private boolean validate(Level parent, String command) {
            if(parent == null) {
                return true;
            }
            if(commands.contains(command)) {
                return false;
            } else {
                return validate(parent.getPreviousLevel(), command);
            }
        }
    }
}
