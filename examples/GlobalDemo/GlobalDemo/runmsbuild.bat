@call rsvars.bat
@C:\Tools\pasdoc\pasdoc.exe --include .\ MainForm.pas ReceiverForm.pas --output GlobalDemo\docs --name DelphiMSBuild2 --write-uses-list
rem @msbuild GlobalDemo.dproj /target:Build /p:config="Release Build"