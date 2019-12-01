from invoke import task, context
import os
import subprocess
from colorama import *
import glob
from shutil import copy2, rmtree, copytree
from datetime import datetime
import pathlib

# import markdown
# from markdown.extensions.tables import TableExtension
# from markdown.extensions.toc import TocExtension
# from markdown.extensions.fenced_code import FencedCodeExtension
# from markdown.extensions.codehilite import CodeHiliteExtension

from pathlib import Path

init()

DEFAULT_DELPHI_VERSION = "10.3"

g_releases_path = "releases"
g_output = "bin"
g_output_folder = ""  # defined at runtime
g_version = 'DEV'


def get_delphi_projects_to_build(which='', delphi_version=DEFAULT_DELPHI_VERSION):
    projects = []
    dversion = 'd' + delphi_version.replace('.', '')
    if not which or which == 'core':
        projects += glob.glob(r"packages\{dversion}\*.groupproj".format(dversion=dversion))
        projects += glob.glob(r"tools\entitygenerator\MVCAREntitiesGenerator.dproj")
    if not which or which == 'tests':
        projects += glob.glob(r"unittests\**\*.dproj")
    if not which or which == 'samples':
        projects += glob.glob(r"samples\**\*.dproj")
        projects += glob.glob(r"samples\**\**\*.dproj")
        projects += glob.glob(r"samples\**\**\**\*.dproj")
    return sorted(projects)


def build_delphi_project(ctx: context.Context, project_filename, config='DEBUG', delphi_version=DEFAULT_DELPHI_VERSION):
    delphi_versions = {
        "XE7": {"path": "15.0", "desc": "Delphi XE7"},
		"XE8": {"path": "16.0", "desc": "Delphi XE8"},
		"10": {"path": "17.0", "desc": "Delphi 10 Seattle"},
        "10.1": {"path": "18.0", "desc": "Delphi 10.1 Berlin"},
        "10.2": {"path": "19.0", "desc": "Delphi 10.2 Tokyo"},
        "10.3": {"path": "20.0", "desc": "Delphi 10.3 Rio"},
    }

    assert delphi_version in delphi_versions, "Invalid Delphi version: " + delphi_version
    print("[" + delphi_versions[delphi_version]["desc"] + "] ", end="")
    version_path = delphi_versions[delphi_version]["path"]

    rsvars_path = f'C:\\Program Files (x86)\\Embarcadero\\Studio\\{version_path}\\bin\\rsvars.bat'
    if not os.path.isfile(rsvars_path):
        rsvars_path = f'D:\\Program Files (x86)\\Embarcadero\\Studio\\{version_path}\\bin\\rsvars.bat'
        if not os.path.isfile(rsvars_path):
            raise Exception("Cannot find rsvars.bat")
    cmdline = '"' + rsvars_path + '"' + " & msbuild /t:Build /p:Config=" + config + " /p:Platform=Win32 \"" + project_filename + "\""
    return ctx.run(cmdline, hide=True, warn=True)


def zip_samples(version):
    global g_output_folder
    cmdline = "7z a " + g_output_folder + f"\\..\\{version}_samples.zip -r -i@7ziplistfile.txt"
    return subprocess.call(cmdline, shell=True) == 0


def create_zip(ctx, version):
    global g_output_folder
    print("CREATING ZIP")
    archive_name = "..\\" + version + ".zip"
    switches = ""
    files_name = "*"
    cmdline = f"..\\..\\7z.exe a {switches} {archive_name} *"
    print(cmdline)
    with ctx.cd(g_output_folder):
        ctx.run(cmdline, hide=False)


def copy_sources():
    global g_output_folder
    os.makedirs(g_output_folder + "\\sources", exist_ok=True)
    os.makedirs(g_output_folder + "\\ideexpert", exist_ok=True)
    os.makedirs(g_output_folder + "\\packages", exist_ok=True)
    os.makedirs(g_output_folder + "\\tools", exist_ok=True)
    # copying main sources
    print("Copying DMVCFramework Sources...")
    src = glob.glob("sources\\*.pas") + glob.glob("sources\\*.inc")
    for file in src:
        print("Copying " + file + " to " + g_output_folder + "\\sources")
        copy2(file, g_output_folder + "\\sources\\")

    # copying tools
    print("Copying tools...")
    copytree('tools\\entitygenerator', g_output_folder + "\\tools\\entitygenerator")
    #copytree('tools\\rql2sql', g_output_folder + "\\tools\\rql2sql")

    # copying ideexperts
    print("Copying DMVCFramework IDEExpert...")
    src = glob.glob("ideexpert\\*.pas") + \
          glob.glob("ideexpert\\*.dfm") + \
          glob.glob("ideexpert\\*.ico") + \
          glob.glob("ideexpert\\*.bmp")

    for file in src:
        print("Copying " + file + " to " + g_output_folder + "\\ideexpert")
        copy2(file, g_output_folder + "\\ideexpert\\")

    files = [
        "dmvcframeworkDTResource.rc",
        "dmvcframework_group.groupproj",
        "dmvcframeworkRT.dproj",
        "dmvcframeworkRT.dpk",
        "dmvcframeworkDT.dproj",
        "dmvcframeworkDT.dpk"
    ]

    folders = [
        "d100",	
        "d101",
        "d102",
        "d103"
    ]

    for folder in folders:
        print(f"Copying DMVCFramework Delphi {folder} packages...")
        for file in files:
            os.makedirs(g_output_folder + f"\\packages\\{folder}", exist_ok=True)
            copy2(rf"packages\{folder}\{file}", g_output_folder + rf"\packages\{folder}")


def copy_libs(ctx):
    global g_output_folder

    
    # swagdoc
    print("Copying libraries: SwagDoc...")
    curr_folder = g_output_folder + "\\lib\\swagdoc"
    os.makedirs(curr_folder, exist_ok=True)
    if not ctx.run(rf"xcopy lib\swagdoc\*.* {curr_folder}\*.* /E /Y /R /V /F"):
        raise Exception("Cannot copy SwagDoc")

    # loggerpro
    print("Copying libraries: LoggerPro...")
    curr_folder = g_output_folder + "\\lib\\loggerpro"
    os.makedirs(curr_folder, exist_ok=True)
    if not ctx.run(rf"xcopy lib\loggerpro\*.* {curr_folder}\*.* /E /Y /R /V /F"):
        raise Exception("Cannot copy loggerpro")

    # dmustache
    print("Copying libraries: dmustache...")
    curr_folder = g_output_folder + "\\lib\\dmustache"
    os.makedirs(curr_folder, exist_ok=True)
    if not ctx.run(rf"xcopy lib\dmustache\*.* {curr_folder}\*.* /E /Y /R /V /F"):
        raise Exception("Cannot copy dmustache")

    # # loggerpro
    # print("Copying libraries: LoggerPro...")
    # curr_folder = g_output_folder + "\\lib\\loggerpro"
    # os.makedirs(curr_folder, exist_ok=True)
    # src = glob.glob("lib\\loggerpro\\*.pas")
    # for file in src:
    #     print("Copying " + file + " to " + curr_folder)
    #     copy2(file, curr_folder)
    # copy2("lib\\loggerpro\\License.txt", curr_folder)
    # copy2("lib\\loggerpro\\VERSION.TXT", curr_folder)
    #
    #
    #
    # # dmustache
    # print("Copying libraries: dmustache...")
    # curr_folder = g_output_folder + "\\lib\\dmustache"
    # os.makedirs(curr_folder, exist_ok=True)
    # src = glob.glob("lib\\dmustache\\*.pas") + \
    #       glob.glob("lib\\dmustache\\*.inc")
    # for file in src:
    #     print("Copying " + file + " to " + curr_folder)
    #     copy2(file, curr_folder)
    # copy2("lib\\dmustache\\README.md", curr_folder)


def printkv(key, value):
    print(Fore.RESET + key + ': ' + Fore.GREEN + value.rjust(60) + Fore.RESET)


def init_build(version):
    """Required by all tasks"""
    global g_version
    global g_output_folder
    global g_releases_path
    g_version = version
    g_output_folder = g_releases_path + "\\" + g_version
    print()
    print(Fore.RESET + Fore.RED + "*" * 80)
    print(Fore.RESET + Fore.RED + " BUILD VERSION: " + g_version + Fore.RESET)
    print(Fore.RESET + Fore.RED + " OUTPUT PATH  : " + g_output_folder + Fore.RESET)
    print(Fore.RESET + Fore.RED + "*" * 80)    

    rmtree(g_output_folder, True)
    os.makedirs(g_output_folder, exist_ok=True)
    f = open(g_output_folder + "\\version.txt", "w")
    f.write("VERSION " + g_version + "\n")
    f.write("BUILD DATETIME " + datetime.now().isoformat() + "\n")
    f.close()
    copy2("README.md", g_output_folder)
    copy2("3_0_0_breaking_changes.md", g_output_folder)
    copy2("3_1_0_breaking_changes.md", g_output_folder)
    copy2("3_2_0_breaking_changes.md", g_output_folder)
    copy2("License.txt", g_output_folder)


def build_delphi_project_list(ctx, projects, config="DEBUG", filter='', delphi_version=DEFAULT_DELPHI_VERSION):
    ret = True
    for delphi_project in projects:
        if filter and (not filter in delphi_project):
            print(f"Skipped {os.path.basename(delphi_project)}")
            continue
        msg = f"Building: {os.path.basename(delphi_project)}  ({config})"
        print(Fore.RESET + msg.ljust(90, '.'), end="")
        res = build_delphi_project(ctx, delphi_project, 'DEBUG', delphi_version)
        if res.ok:
            print(Fore.GREEN + 'OK' + Fore.RESET)
        else:
            ret = False
            print(Fore.RED + "\n\nBUILD ERROR")
            print(Fore.RESET + res.stdout)
            print("\n")

    return ret


@task
def tests(ctx, delphi_version=DEFAULT_DELPHI_VERSION):
    """Builds and execute the unit tests"""
    import os
    apppath = os.path.dirname(os.path.realpath(__file__))
    res = True
    tests = [
        r"unittests\serializer\jsondataobjects\TestSerializerJsonDataObjects.dproj"
    ]
    testsexe = [
        r"unittests\serializer\jsondataobjects\Win32\CI\TestSerializerJsonDataObjects.exe"
    ]
    i = 0
    for test_project in tests:
        res = build_delphi_project(ctx, test_project, 'CI', delphi_version) and res
        if res:
            exename = apppath + "\\" + testsexe[i]
            printkv("Running", exename)
            res = ctx.run(exename, hide=False)
            if not res:
                print("UnitTest execution failed!")
                return False
            i = i + 1
    return res


@task
def clean(ctx):
    global g_output_folder
    import os
    import glob
    print(f"Cleaning from {g_output_folder}...")

    output = pathlib.Path(g_output_folder)

    rmtree(g_output_folder + r"\lib\loggerpro\Win32", True)
    rmtree(g_output_folder + r"\lib\loggerpro\packages\d101\__history", True)
    rmtree(g_output_folder + r"\lib\loggerpro\packages\d101\Win32\Debug", True)
    rmtree(g_output_folder + r"\lib\loggerpro\packages\d102\__history", True)
    rmtree(g_output_folder + r"\lib\loggerpro\packages\d102\Win32\Debug", True)
    rmtree(g_output_folder + r"\lib\loggerpro\packages\d103\__history", True)
    rmtree(g_output_folder + r"\lib\loggerpro\packages\d103\Win32\Debug", True)
    rmtree(g_output_folder + r"\lib\dmustache\.git", True)
    rmtree(g_output_folder + r"\lib\swagdoc\lib", True)
    rmtree(g_output_folder + r"\lib\swagdoc\deploy", True)
    rmtree(g_output_folder + r"\lib\swagdoc\demos", True)

    to_delete = []
    to_delete += glob.glob(g_output_folder + r"\**\*.exe", recursive=True)
    to_delete += glob.glob(g_output_folder + r"\**\*.dcu", recursive=True)
    to_delete += glob.glob(g_output_folder + r"\**\*.stat", recursive=True)
    to_delete += glob.glob(g_output_folder + r"\**\*.res", recursive=True)
    to_delete += glob.glob(g_output_folder + r"\**\*.map", recursive=True)
    to_delete += glob.glob(g_output_folder + r"\**\*.~*", recursive=True)
    to_delete += glob.glob(g_output_folder + r"\**\*.rsm", recursive=True)
    to_delete += glob.glob(g_output_folder + r"\**\*.drc", recursive=True)
    to_delete += glob.glob(g_output_folder + r"\**\*.log", recursive=True)
    to_delete += glob.glob(g_output_folder + r"\**\*.local", recursive=True)
    to_delete += glob.glob(g_output_folder + r"\**\*.gitignore", recursive=True)
    to_delete += glob.glob(g_output_folder + r"\**\*.gitattributes", recursive=True)

    for f in to_delete:
        os.remove(f)


@task(pre=[tests])
def release(ctx, version="DEBUG", delphi_version=DEFAULT_DELPHI_VERSION, skip_build=False):
    """Builds all the projects, executes integration tests and prepare the release"""
    init_build(version)
    if not skip_build:
        delphi_projects = get_delphi_projects_to_build('', delphi_version)
        if not build_delphi_project_list(ctx, delphi_projects, version, '', delphi_version):
            return False #fails build
    print(Fore.RESET)
    copy_sources()
    copy_libs(ctx)
    clean(ctx)
    zip_samples(version)
    create_zip(ctx, version)


@task
def build_samples(ctx, version="DEBUG", filter="", delphi_version=DEFAULT_DELPHI_VERSION):
    """Builds samples"""
    init_build(version)
    delphi_projects = get_delphi_projects_to_build('samples', delphi_version)
    return build_delphi_project_list(ctx, delphi_projects, version, filter, delphi_version)


@task
def build_core(ctx, version="DEBUG", delphi_version=DEFAULT_DELPHI_VERSION):
    """Builds core packages extensions"""
    init_build(version)
    delphi_projects = get_delphi_projects_to_build('core', delphi_version)
    return build_delphi_project_list(ctx, delphi_projects, version, '', delphi_version)
