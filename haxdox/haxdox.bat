@echo off
rem to run from haxtatic/src/main.hs' folder

del /F /Q /S ..\docs\*
del /F /Q /S ..\haxdox\build\*
stack runghc main.hs ..\haxdox
xcopy ..\haxdox\build\* ..\docs\ /S /I /Q /Y
