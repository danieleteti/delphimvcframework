# coding: latin-1
import subprocess
import os
import WConio
import glob

projects = glob.glob("*\**\*.dproj")

#################################################################################
def buildProject(project):
    print "Building " + project
    p = project.replace('.dproj', '.cfg')
    if os.path.isfile(p):
		if os.path.isfile(p + '.unused'):
			os.remove(p + '.unused')
		os.rename(p, p + '.unused')
    # print os.system("msbuild /t:Build /p:Config=Debug \"" + project + "\"")
    return subprocess.call("rsvars.bat & msbuild /t:Build /p:Config=Debug /p:Platform=Win32 \"" + project + "\"",
                           shell=True) == 0


def summaryTable(builds):
    WConio.clrscr()
    WConio.textcolor(WConio.WHITE)
    print "PROJECT NAME".ljust(70) + "STATUS".ljust(10)
    print "=" * 80
    good = bad = 0
    for item in builds:
        if item['status'] == 'ok':
            WConio.textcolor(WConio.LIGHTGREEN)
            good += 1
        else:
            WConio.textcolor(WConio.RED)
            bad += 1
        print item['project'].ljust(70) + item['status'].ljust(4)
    WConio.textcolor(WConio.WHITE)
    print "=" * 80
    WConio.textcolor(WConio.GREEN)
    print "GOOD :".rjust(70) + str(good).rjust(10, '.')
    WConio.textcolor(WConio.RED)
    print "BAD  :".rjust(70) + str(bad).rjust(10, '.')


#################################################################################

def main():
    builds = []
    for project in projects:
        filename = '\\'.join(project.split('\\')[-3:])
        list = {'project': filename}
        if buildProject(project):
            list["status"] = "ok"
        else:
            list["status"] = "ko"
        builds.append(list)
    summaryTable(builds)

# Store current attribute settings
old_setting = WConio.gettextinfo()[4] & 0x00FF

main()

# Restore old attribute settings
WConio.textattr(old_setting)