@echo off

dotnet aardpack --parseonly > __version.txt
set /p VERSION=<__version.txt
del __version.txt

dotnet pack src\FShade.Preprocessor\fshadeaot.fsproj -c Release --no-build -p:Version=%VERSION% -o bin\pack