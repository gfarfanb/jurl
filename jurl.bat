@echo off

for /F "delims=" %%G in ('dir /b /s jurl-*.jar') do set jurl=%%~G

for %%A in ("%jurl%") do set path=%%~dpA

java -cp "%path%" -jar "%jurl%" %*
