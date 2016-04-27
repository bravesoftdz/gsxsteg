@echo off

call build.bat
if exist gsxsteg-win32.zip del gsxsteg-win32.zip
"%PROGRAMFILES%\7-ZIP\7z.exe" a -tzip -mx gsxsteg-win32.zip @binlist

call buildw64.bat
if exist gsxsteg-win64.zip del gsxsteg-win64.zip
"%PROGRAMFILES%\7-ZIP\7z.exe" a -tzip -mx gsxsteg-win64.zip @binlist
