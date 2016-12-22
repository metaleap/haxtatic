@echo off
rem to run from haxtatic folder
rem all the `.\` is of course superfluous but nvm

rem del /F /Q /S .\src-docs\build\*
del /F /Q /S .\docs\*
stack .\@HaXtatic.hs .\src-docs default.haxproj overwrites.haxproj
xcopy .\src-docs\build\* .\docs\ /S /I /Q /Y
