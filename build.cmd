@echo off
dotnet tool restore
dotnet paket restore
dotnet build src\FShade.sln --configuration Release
dotnet test src\FShade.sln --no-build --configuration Release
