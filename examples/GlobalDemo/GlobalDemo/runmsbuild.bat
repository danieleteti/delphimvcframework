@call rsvars.bat
@pasdoc.exe --output "C:\Users\Daniele\.hudson\jobs\delphimsbuild2\workspace\GlobalDemo\docs" --name DelphiMSBuild2 --write-uses-list MainForm.pas ReceiverForm.pas 
rem @msbuild GlobalDemo.dproj /target:Build /p:config="Release Build"