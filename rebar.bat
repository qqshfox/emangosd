@echo off
setlocal
set rebarscript=%~f0
echo %rebarscript
escript.exe "%rebarscript:.bat=%" %*
