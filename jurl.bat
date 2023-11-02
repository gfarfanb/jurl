@echo off

if not "%JURL_HOME%"=="" goto jurlHomeFound
set "JURL_PATTERN=jurl-*.jar"
goto existsJurl

:jurlHomeFound
set "JURL_PATTERN=%JURL_HOME%\jurl-*.jar"

:existsJurl
for /F "delims=" %%G in ('dir /b /s %JURL_PATTERN%') do (
    set JURL_CMD=%%~G
    goto validateJava
)
echo 'jurl-<version>.jar' artifact not found. >&2
echo Download the latest artifact from https://github.com/gfarfanb/jurl/packages >&2
exit /b 1

:validateJava
if not "%JAVA_HOME%"=="" goto javaHomeFound
for %%i in (java.exe) do set "JAVA_CMD=%%~$PATH:i"
goto existsJava

:javaHomeFound
set "JAVA_CMD=%JAVA_HOME%\bin\java.exe"

:existsJava
if exist "%JAVA_CMD%" goto executeJurl
echo 'java' command is not installed or >&2
echo JAVA_HOME environment variable is not defined correctly >&2
exit /b 1

:executeJurl
"%JAVA_CMD%" -jar "%JURL_CMD%" %* >&2
