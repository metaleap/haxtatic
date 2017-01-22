@echo off
rem TO RUN ONLY FROM haxtatic/src folder!!

rem del /F /Q /S ..\dox-demo\default-build\*
rem del /F /Q /S ..\docs\*
stack .\@HaxMain.hs ..\dox-demo
rem xcopy ..\dox-demo\default-build\* ..\docs\ /S /I /Q /Y
