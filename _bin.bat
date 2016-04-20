@echo off

if exist gsxsteg-win32.zip del gsxsteg-win32.zip
"%PROGRAMFILES%\7-ZIP\7z.exe" a -tzip -mx gsxsteg-win32.zip @binlist
