@call rsvars.bat
@C:\Tools\pasdoc\pasdoc.exe --include .\ MainForm.pas ReceiverForm.pas --output docs --name DelphiMSBuild2 --write-uses-list
@msbuild GlobalDemo.dproj /target:Build /p:config="Release Build"