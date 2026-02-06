@echo off
setlocal enabledelayedexpansion

set "TARGET=    <Target Name=\"PreBuildEvent\" BeforeTargets=\"Build\">^

        <Message Text=\"Compiling template resources...\" Importance=\"high\"/>^

        <Exec Command=\"brcc32 ^^^&quot;$(MSBuildProjectDirectory)\..\..\ideexpert\DMVC.Expert.Templates.rc^^^&quot;\" WorkingDirectory=\"$(MSBuildProjectDirectory)\..\..\ideexpert\"/>^

    </Target>"

for %%d in (d110 d104 d103 d102 d101 d100) do (
    echo Processing %%d...
    powershell -Command "(Get-Content '%%d\dmvcframeworkDT.dproj') -replace '    </ProjectExtensions>', ('    </ProjectExtensions>' + [Environment]::NewLine + '    <Target Name=\"PreBuildEvent\" BeforeTargets=\"Build\">' + [Environment]::NewLine + '        <Message Text=\"Compiling template resources...\" Importance=\"high\"/>' + [Environment]::NewLine + '        <Exec Command=\"brcc32 ^^^&quot;$(MSBuildProjectDirectory)\..\..\ideexpert\DMVC.Expert.Templates.rc^^^&quot;\" WorkingDirectory=\"$(MSBuildProjectDirectory)\..\..\ideexpert\"/>' + [Environment]::NewLine + '    </Target>') | Set-Content '%%d\dmvcframeworkDT.dproj.new' -Encoding UTF8"
    move /y "%%d\dmvcframeworkDT.dproj.new" "%%d\dmvcframeworkDT.dproj"
)

echo Done!
