@call rsvars.bat
@pasdoc.exe --output docs --name DelphiMSBuild2 --write-uses-list MainForm.pas ReceiverForm.pas 
rem @msbuild GlobalDemo.dproj /target:Build /p:config="Release Build"