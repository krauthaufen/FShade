@echo off
dotnet tool restore
dotnet paket restore

FOR /F "tokens=* USEBACKQ" %%F IN (`dotnet aardpack --parseonly`) DO (SET VERSION=%%F)
dotnet aardpack src\FShade.sln --notag
dotnet pack .\src\FShade.Preprocessor\fshadeaot.fsproj --no-build -c Release -o bin\pack /p:Version=%VERSION%