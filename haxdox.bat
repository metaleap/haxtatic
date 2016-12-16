@echo off
rem to run from hastatic/src/main.hs' folder

del /F /S /Q ..\docs\haxdox\build\*
stack runghc main.hs ..\docs\haxdox
del /F /Q ..\docs\*.*
xcopy ..\docs\haxdox\build\* ..\docs\ /S /I /Q /Y
