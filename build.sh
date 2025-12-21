#!/bin/bash
dotnet tool restore
dotnet paket restore
dotnet build src/FShade.sln
dotnet test src/FShade.sln --no-build