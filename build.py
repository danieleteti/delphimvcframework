# coding: latin-1
import subprocess
import os
import glob
from colorama import *

init()


#################################################################################
def buildProject(project, platform = 'Win32'):
	print(Fore.YELLOW + "Building " + project)
	p = project.replace('.dproj', '.cfg')
	if os.path.isfile(p):
		if os.path.isfile(p + '.unused'):
			os.remove(p + '.unused')
		os.rename(p, p + '.unused')
	# print os.system("msbuild /t:Build /p:Config=Debug \"" + project + "\"")
	return subprocess.call("rsvars.bat & msbuild /t:Build /p:Config=Debug /p:Platform=" + platform + " \"" + project + "\"", shell=True) == 0


def summaryTable(builds):
	print(ansi.clear_screen())
	dmvc_copyright()
	print(Fore.WHITE + "PROJECT NAME".ljust(80) + "STATUS".ljust(10))
	print(Fore.YELLOW + "=" * 90)
	good = bad = 0
	for item in builds:
		if item['status'].startswith('ok'):
			good += 1
		else:
			bad += 1
		print(Fore.BLUE + item['project'].ljust(80) + (Fore.WHITE if item['status'].startswith('ok') else Fore.RED) + item['status'].ljust(4))
				
	print(Fore.YELLOW + "=" * 90)
	print(Fore.WHITE + "GOOD :".rjust(80) + str(good).rjust(10, '.'))
	print(Fore.RED + "BAD  :".rjust(80) + str(bad).rjust(10, '.'))


#################################################################################

def main(projects):
	dmvc_copyright()
	builds = []
	for project in projects:
			filename = '\\'.join(project.split('\\')[-3:])
			list = {'project': filename}
			if project.find('delphistompclient') > -1 or project.find('contribsamples') > -1:
					list['status'] = 'skip'
					continue
			
			list = {'project': filename}
			if buildProject(project):
					list["status"] = "ok"
			else:
					list["status"] = "ko"
			builds.append(list)

			if (os.path.exists(project + '.android')):
					list = {'project': filename}
					if buildProject(project, 'Android'):
							list["status"] = "okandroid"
					else:
							list["status"] = "koandroid"
					builds.append(list)  				
	summaryTable(builds)

# Store current attribute settings
#old_setting = WConio.gettextinfo()[4] & 0x00FF

def dmvc_copyright():
  print(Style.BRIGHT + Fore.WHITE + "----------------------------------------------------------------------------------------")	
  print(Fore.RED + "                 ** Delphi MVC Framework Building System **")
  print(Fore.WHITE + "Delphi MVC Framework is CopyRight (2010-2017) of Daniele Teti and the DMVCFramework TEAM")
  print(Fore.RESET + "----------------------------------------------------------------------------------------\n")

## MAIN ##
projects = glob.glob("ideexpert\*.dproj")
projects += glob.glob("unittests\**\*.dproj")
projects += glob.glob("*\**\*.dproj")
projects += glob.glob("*\**\**\*.dproj")
main(projects)
print(Style.RESET_ALL)