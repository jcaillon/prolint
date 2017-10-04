@echo off

rmdir /q /s "%~dp0\.buildtmp" 1>nul 2>nul
del /q "%~dp0\prolint.zip" 1>nul 2>nul

mkdir "%~dp0\.buildtmp\prolint" 1>nul 2>nul
robocopy "%~dp0\" "%~dp0\.buildtmp\prolint" *.* /XD ".git" ".buildtmp" /XF ".gitignore" "build.bat" /S

mkdir "%~dp0\build" 1>nul 2>nul
7z a "%~dp0\build\prolint.zip" "%~dp0\.buildtmp\*"

rmdir /q /s "%~dp0\.buildtmp" 1>nul 2>nul

echo ======================================
echo DONE : prolint.zip created in the build directory
echo ======================================

pause