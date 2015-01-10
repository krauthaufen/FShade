@echo off
echo %~dp0

PUSHD %~dp0
cls

IF exist packages\FAKE ( echo skipping FAKE download ) ELSE ( 
echo downloading FAKE
REM mklink .\.git\hooks\pre-commit .\pre-commit
"Packages\nuget.exe" "install" "FAKE" "-OutputDirectory" "Packages" "-ExcludeVersion" "-Prerelease"
"Packages\nuget.exe" "install" "FSharp.Formatting.CommandTool" "-OutputDirectory" "Packages" "-ExcludeVersion" "-Prerelease"
"Packages\nuget.exe" "install" "SourceLink.Fake" "-OutputDirectory" "Packages" "-ExcludeVersion"
"Packages\nuget.exe" "install" "NUnit.Runners" "-OutputDirectory" "Packages" "-ExcludeVersion"
)

SET TARGET="Default"

IF NOT [%1]==[] (set TARGET="%1")

"Packages\FAKE\tools\Fake.exe" "build.fsx" "target=%TARGET%"

REM IF NOT [%1]==[] (set TARGET="%1")
REM "tools\FAKE\tools\Fake.exe" "build.fsx" "target=%TARGET%" %*