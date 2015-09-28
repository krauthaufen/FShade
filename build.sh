#!/bin/bash

if [ ! -d "packages/FAKE" ]; then
	echo "downloading FAKE"
	mono --runtime=v4.0 bin/nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion -Version 3.35.2
	mono --runtime=v4.0 bin/nuget.exe install Paket.Core -OutputDirectory packages -ExcludeVersion -Version 1.18.5
fi

wget -q --no-check-certificate https://github.com/vrvis/Aardvark.Fake/raw/master/bin/Aardvark.Fake.dll -O bin/Aardvark.Fake.dll

mono packages/FAKE/tools/FAKE.exe "build.fsx"  $@

