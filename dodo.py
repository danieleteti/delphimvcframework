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
projects += glob.glob("*\**\*.dproj")
projects += glob.glob("*\**\**\*.dproj")

# projects = [
#             tools_project,
#             #CLOUD SUPPORT
#             'IDSource\Moduli\H2W_Cloud\h2wproxy\H2WProxy.dproj',
#             'IDSource\Moduli\H2W_Cloud\h2wcloudserver\H2WCloudServer.dproj',
#             'IDSource\IDServer\IDServer.dproj',
#             #END-CLOUD SUPPORT
#             'IDSource\pepeFile\DataManager.dproj',
#             'IDSource\PepeSetup\SetupManager.dproj',
#             'IDSource\POS\POS.dproj',
#             'IDSource\IBBackup\IBBackup.dproj',
#             'IDSource\IBSetup\IBSetup.dproj',
#             'IDSource\IBUpdate\IBUpdate.dproj',
#             'IDSource\Telefonia\PhoneManager\PhoneManager.dproj',
#             'IDSource\Telefonia\Multicarrier\IDCallAccountEditor.dproj',
#             'IDSource\Telefonia\IDSerial\IDSerial.dproj',
#             'IDSource\Telefonia\IDCsta\IDCsta.dproj',
#             'IDSource\Telefonia\IDCsta\IDHotelCsta\IDHotelCSTA.dproj',
#             'IDSource\Telefonia\CilHotel\RX\CilHotelRX.dproj',
#             'IDSource\Telefonia\CilHotel\TX\CilHotelTX.dproj',
#             'IDSource\Telefonia\IDSAE3000\IDSAE3000.dproj',
#             'IDSource\Telefonia\IDCsta\IDWatchDog\IDWatchDog.dproj',
#             'IDSource\DLLDocumenti\EDW2003.dproj',
#             'IDSource\DLLDocumenti\WordDocsDLL.dproj',
#             'IDSource\pepe\Reception.dproj',
#             'IDSource\Cont\Step1\pjHGWnew.dproj',
#             'IDSource\Moduli\H2W\server\H2Ws.dproj',
#             'IDSource\Personalizer\Personalizer.dproj',
#             'IDSource\Boffice\BackOffice.dproj'
#             ]
releases_path = "releases"
output = "bin"
OutputFolder = ""  # defined ar runtime
innosetupcompiler = "\"C:\\Program Files (x86)\\Inno Setup 5\\ISCC.exe\""
OUTPUT_SFX_FILENAME = releases_path + "\\" + 'HOAsys_{}.exe'
OUTPUT_CLOUD_SFX_FILENAME = releases_path + "\\" + 'HOAsys_Cloud_{}.exe'
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

    print(Style.BRIGHT + Back.WHITE + Fore.RED + "*" * 70 + Style.RESET_ALL)
    for txt in elements:
        s = '{:^70}'.format(txt)
        print(Style.BRIGHT + Back.WHITE + Fore.RED + s + Style.RESET_ALL)
    print(Style.BRIGHT + Back.WHITE + Fore.RED + "*" * 70 + Style.RESET_ALL)


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
    for project in projects:
        res = buildProject(project, config)
        if not res:
            return False
    return True


def copy_sources():
    global OutputFolder
    os.makedirs(OutputFolder + "\\sources", exist_ok=True)
    os.makedirs(OutputFolder + "\\ideexpert", exist_ok=True)
    # copying main sources
    header("Copying DMVCFramework Sources...")
    src = glob.glob("sources\\*.pas") + glob.glob("sources\\*.inc")
    for file in src:
        print("Copying " + file + " to " + OutputFolder + "\\sources")
        copy2(file, OutputFolder + "\\sources\\")
    copy2("lib\\jsondataobjects\\Source\\JsonDataObjects.pas",
          OutputFolder + "\\sources\\")

    # copying ideexperts
    header("Copying DMVCFramework IDEExpert...")
    print("Copying ideexpert")
    src = glob.glob("ideexpert\\*.pas") + \
        glob.glob("ideexpert\\*.dfm") + glob.glob("ideexpert\\*.ico")
    src += glob.glob("ideexpert\\*.dpk") + glob.glob("ideexpert\\*.dproj")
    for file in src:
        print("Copying " + file + " to " + OutputFolder + "\\ideexpert")
        copy2(file, OutputFolder + "\\ideexpert\\")


def copy_libs():
    global OutputFolder

    # loggerpro
    header("Copying libraries: LoggerPro...")
    curr_folder = OutputFolder + "\\lib\\loggerpro"
    os.makedirs(curr_folder, exist_ok=True)
    src = glob.glob("lib\\loggerpro\\*.pas")
    for file in src:
        print("Copying " + file + " to " + curr_folder)
        copy2(file, curr_folder)
    copy2("lib\\loggerpro\\LICENSE", curr_folder)
    copy2("lib\\loggerpro\\VERSION.TXT", curr_folder)

    # dmustache
    header("Copying libraries: dmustache...")
    curr_folder = OutputFolder + "\\lib\\dmustache"
    os.makedirs(curr_folder, exist_ok=True)
    src = glob.glob("lib\\dmustache\\*.pas") + \
        glob.glob("lib\\dmustache\\*.inc")
    for file in src:
        print("Copying " + file + " to " + curr_folder)
        copy2(file, curr_folder)
    copy2("lib\\dmustache\\README.md", curr_folder)


def create_build_tag(version):
    global GlobalBuildVersion
    global OutputFolder
    global releases_path
    GlobalBuildVersion = version
    OutputFolder = releases_path + "\\" + version
    print('Output path: ' + OutputFolder)
    header("BUILD VERSION: " + GlobalBuildVersion)
    rmtree(OutputFolder, True)    
    os.makedirs(OutputFolder, exist_ok=True)
    f = open(OutputFolder + "\\version.txt", "w")
    f.write("VERSION " + GlobalBuildVersion + "\n")
    f.write("BUILD DATETIME " + datetime.now().isoformat() + "\n")
    f.close()
    copy2("README.md", OutputFolder)
    copy2("3_0_0_breaking_changes.md", OutputFolder)
    copy2("roadmap.md", OutputFolder)
    copy2("LICENSE", OutputFolder)


def compileSetup():
    return True
    header("BUILDING TOOLS SETUP")
    return subprocess.call(innosetupcompiler + " tools\hoasys_datamover.iss", shell=False) == 0


def createsfx():
    return True
    global GlobalBuildVersion
    header("CREATING SFX")
    cmdline = "scripts\\7z.exe a -sfxscripts\\7z.sfx " + OUTPUT_SFX_FILENAME.format(
        GlobalBuildVersion) + " bin\\*.exe bin\\*.dll bin\\*.msg bin\\*.ini bin\\HOASYS-BUILD-TIMESTAMP.TXT bin\\i18n\\*.sil bin\\i18n\\*.dic"
    return subprocess.call(cmdline, shell=True) == 0


def createcloudsfx():
    return True
    global GlobalBuildVersion
    header("CREATING CLOUDBIN SFX")
    cmdline = "scripts\\7z.exe a -sfxscripts\\7z.sfx " + OUTPUT_CLOUD_SFX_FILENAME.format(
        GlobalBuildVersion) + " cloudbin\\*.exe cloudbin\\*.dbmap cloudbin\\*.ini cloudbin\\HOASYS-BUILD-TIMESTAMP.TXT"
    return subprocess.call(cmdline, shell=True) == 0


##########################################################################

def task_build():
    '''Use: doit build -v <VERSION>. Then creates SFX archive.'''
    return {
        'actions': [
            create_build_tag,
            # "echo %%date%% %%time:~0,8%% > " + releases_path + "\\DELPHIMVCFRAMEWORK-BUILD-TIMESTAMP.TXT",
            copy_sources,
            copy_libs,
            compileSetup,
            createsfx,
            createcloudsfx],
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


def task_sfx():
    '''creates the SFX self extractable archive from the contents of \\bin and \\cloudbin'''
    return {
        'actions': ['echo off && copy bin\\HOASYS-BUILD-TIMESTAMP.TXT cloudbin', createsfx, createcloudsfx]
    }


# def task_tools():
#     '''creates the tools setup'''
#     return {
#         'actions': [(buildProject, [tools_project]), compileSetup]
#     }
