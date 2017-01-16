@echo off
rem to run from haxtatic/src folder

rem del /F /Q /S ..\dox-demo\build\*
rem del /F /Q /S ..\docs\*
stack .\@HaxMain.hs ..\dox-demo
rem doxdemo.haxproj overwrites.haxproj posts.haxproj
rem xcopy ..\dox-demo\build\* ..\docs\ /S /I /Q /Y
