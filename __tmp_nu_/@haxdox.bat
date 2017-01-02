@echo off
rem to run from haxtatic folder
rem all the `.\` is of course superfluous but nvm

rem del /F /Q /S .\dox-demo\build\*
rem del /F /Q /S .\docs\*
stack .\@HaXtatic.hs .\dox-demo doxdemo.haxproj overwrites.haxproj
rem xcopy .\dox-demo\build\* .\docs\ /S /I /Q /Y
