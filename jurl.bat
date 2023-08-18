@echo off

for /F "delims=" %%G in ('dir /b /s jurl-*.jar') do java -jar "%%G" %*
