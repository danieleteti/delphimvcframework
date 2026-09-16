@echo off
REM Audit Release config flags of every .dproj in the repo.
REM Focuses on the packages (packages\d*\*.dproj) where wrong flags hurt most.
python "%~dp0audit_release_flags.py" "%~dp0..\..\packages"
