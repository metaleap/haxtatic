@echo off
rem to run from haxtatic/src/main.hs' folder

del /F /Q /S ..\docs\*
del /F /Q /S ..\src-docs\build\*
stack runghc main.hs ..\src-docs
xcopy ..\src-docs\build\* ..\docs\ /S /I /Q /Y
