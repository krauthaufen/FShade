#!/bin/bash

if [ ! -d "Packages/FAKE" ]; then
	echo "downloading FAKE"
	mono --runtime=v4.0 Packages/nuget.exe install FAKE -OutputDirectory Packages -ExcludeVersion
	mono --runtime=v4.0 Packages/nuget.exe install FSharp.Formatting.CommandTool -OutputDirectory Packages -ExcludeVersion -Prerelease 
	mono --runtime=v4.0 Packages/nuget.exe install SourceLink.Fake -OutputDirectory Packages -ExcludeVersion 
fi


mono Packages/FAKE/tools/FAKE.exe "build.fsx"  $@

