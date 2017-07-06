https://docs.microsoft.com/en-us/visualstudio/msbuild/walkthrough-creating-an-msbuild-project-file-from-scratch

Minimal csproj
msbuild ProjectPhase1.csproj /t:Build

Add Build Property
msbuild ProjectPhase2.csproj /t:Build

Clean & Rebuild
msbuild ProjectPhase3.csproj /t:Rebuild

Building Incrementally
msbuild ProjectPhase4.csproj /v:d

