import glob
import os
import subprocess
from shutil import copy2, rmtree
from datetime import datetime

from colorama import *

init()  # colorama initialization

# task setup env
DOIT_CONFIG = {'verbosity': 2, 'default_tasks': ['build']}

##########################################################################
############## CONFIGURATION #############################################
##########################################################################
projects = glob.glob("ideexpert\*.dproj")
projects += glob.glob("unittests\**\*.dproj")
projects += glob.glob("samples\**\*.dproj")

#projects += glob.glob("*\**\**\*.dproj")

releases_path = "releases"
output = "bin"
output_folder = ""  # defined at runtime
##########################################################################
############## END CONFIGURATION #########################################
##########################################################################

# if we are building an actual release, this will be replaced
GlobalBuildVersion = 'DEV'

def header(headers):
    elements = None
    if type(headers).__name__ == 'str':
        elements = [headers]
    else:
        elements = headers

    print(Style.BRIGHT + Back.WHITE + Fore.RED + "*" * 78 + Style.RESET_ALL)
    for txt in elements:
        s = '{:^78}'.format(txt)
        print(Style.BRIGHT + Back.WHITE + Fore.RED + s + Style.RESET_ALL)
    print(Style.BRIGHT + Back.WHITE + Fore.RED + "*" * 78 + Style.RESET_ALL)


def buildProject(project, config='DEBUG'):
    header(["Building", project, "(config " + config + ")"])
    p = project.replace('.dproj', '.cfg')
    if os.path.isfile(p):
        if os.path.isfile(p + '.unused'):
            os.remove(p + '.unused')
        os.rename(p, p + '.unused')
    rsvars_path = 'C:\\Program Files (x86)\\Embarcadero\\Studio\\19.0\\bin\\rsvars.bat'
    if not os.path.isfile(rsvars_path):
        rsvars_path = 'D:\\Program Files (x86)\\Embarcadero\\Studio\\19.0\\bin\\rsvars.bat'
        if not os.path.isfile(rsvars_path):
            return False
    return subprocess.call('"' + rsvars_path + '"' + " & msbuild /t:Build /p:Config=" + config + " /p:Platform=Win32 \"" + project + "\"", shell=True) == 0


def buildProjects(config='RELEASE'):
    res = True
    for project in projects:
        res = buildProject(project, config) and res
    return res

def copy_sources():
    global output_folder
    os.makedirs(output_folder + "\\sources", exist_ok=True)
    os.makedirs(output_folder + "\\ideexpert", exist_ok=True)
    # copying main sources
    header("Copying DMVCFramework Sources...")
    src = glob.glob("sources\\*.pas") + glob.glob("sources\\*.inc")
    for file in src:
        print("Copying " + file + " to " + output_folder + "\\sources")
        copy2(file, output_folder + "\\sources\\")
    # copy2("lib\\jsondataobjects\\Source\\JsonDataObjects.pas",
    #       output_folder + "\\sources\\")

    # copying ideexperts
    header("Copying DMVCFramework IDEExpert...")
    print("Copying ideexpert")
    src = glob.glob("ideexpert\\*.pas") + \
          glob.glob("ideexpert\\*.dfm") + \
					glob.glob("ideexpert\\*.ico") + \
					glob.glob("ideexpert\\*.bmp") + \
					glob.glob("ideexpert\\*.dpk") + \
					glob.glob("ideexpert\\*.dproj")
    for file in src:
        print("Copying " + file + " to " + output_folder + "\\ideexpert")
        copy2(file, output_folder + "\\ideexpert\\")


def copy_libs():
    global output_folder

    # loggerpro
    header("Copying libraries: LoggerPro...")
    curr_folder = output_folder + "\\lib\\loggerpro"
    os.makedirs(curr_folder, exist_ok=True)
    src = glob.glob("lib\\loggerpro\\*.pas")
    for file in src:
        print("Copying " + file + " to " + curr_folder)
        copy2(file, curr_folder)
    copy2("lib\\loggerpro\\LICENSE", curr_folder)
    copy2("lib\\loggerpro\\VERSION.TXT", curr_folder)

    # dmustache
    header("Copying libraries: dmustache...")
    curr_folder = output_folder + "\\lib\\dmustache"
    os.makedirs(curr_folder, exist_ok=True)
    src = glob.glob("lib\\dmustache\\*.pas") + \
        glob.glob("lib\\dmustache\\*.inc")
    for file in src:
        print("Copying " + file + " to " + curr_folder)
        copy2(file, curr_folder)
    copy2("lib\\dmustache\\README.md", curr_folder)


def init_build(version):
    '''Required by all tasks'''
    global GlobalBuildVersion
    global output_folder
    global releases_path
    GlobalBuildVersion = version
    output_folder = releases_path + "\\" + version
    print('Output path: ' + output_folder)
    header("BUILD VERSION: " + GlobalBuildVersion)
    rmtree(output_folder, True)
    os.makedirs(output_folder, exist_ok=True)
    f = open(output_folder + "\\version.txt", "w")
    f.write("VERSION " + GlobalBuildVersion + "\n")
    f.write("BUILD DATETIME " + datetime.now().isoformat() + "\n")
    f.close()
    copy2("README.md", output_folder)
    copy2("3_0_0_breaking_changes.md", output_folder)
    copy2("roadmap.md", output_folder)
    copy2("LICENSE", output_folder)


def zip_samples(version):
    global output_folder
    cmdline = "7z a " + output_folder + "\\dmvcframeworksamples.zip -r -i@7ziplistfile.txt"
    return subprocess.call(cmdline, shell=True) == 0

def create_zip(version):
    global output_folder        
    header("CREATING ZIP")
    archivename = "..\\dmvcframework_" + version + ".zip"
    #switches = " -o.\\" + releases_path + " -w.\\" + output_folder
    switches = ""
    #switches = " -oD:\\DEV\\dmvcframework\\releases\\" + " -w " + output_folder
    #filenames = output_folder + "\\*"
    filenames = "*"
    cmdline = "cd " + output_folder + " & 7z.exe a " + \
        switches + " " + archivename + " " + filenames
    print(cmdline)
    return subprocess.call(cmdline, shell=True) == 0


##########################################################################

def task_build():
    '''Use: doit build -v <VERSION>.'''
    return {
        'actions': [
            buildProjects,
            init_build,
            copy_sources,
            copy_libs,
            zip_samples,
            create_zip],
        'params': [{'name': 'version',
                    'short': 'v',
                    'long': 'version',
                    'type': str,
                    'default': 'DEVELOPMENT'},
                   {'name': 'config',
                    'short': 'c',
                    'long': 'config',
                    'type': str,
                    'default': 'DEBUG'}
                   ],
        'verbosity': 2
    }


def task_zip():
    '''Use: doit zip -v <VERSION>.'''
    return {
        'actions': [
            create_zip],
        'params': [{'name': 'version',
                    'short': 'v',
                    'long': 'version',
                    'type': str,
                    'default': 'DEVELOPMENT'},
                   {'name': 'config',
                    'short': 'c',
                    'long': 'config',
                    'type': str,
                    'default': 'DEBUG'}
                   ],
        'verbosity': 2
    }


def task_buildlight():
    '''Use: doit buildlight -> Builds all the projects.'''
    return {
        'actions': [
            'echo off && touch bin\\x.exe && del bin\\*.exe',
            buildProjects],
        'verbosity': 2
    }


def task_samples():
    '''creates the samples zip file'''
    return {
        'actions': [init_build, zip_samples],
        'params': [
                    {
                        'name': 'version',
                        'short': 'v',
                        'long': 'version',
                        'type': str,
                        'default': 'DEVELOPMENT'
                    }
                  ]        
    }
