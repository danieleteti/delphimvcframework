from invoke import task, context
import os
import subprocess
from colorama import *
import glob
from shutil import copy2, rmtree
from datetime import datetime

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
    dversion = 'd' + delphi_version.replace('.','')
    if not which or which == 'core':
        projects += glob.glob(r"packages\{dversion}\*.groupproj".format(dversion=dversion))
    if not which or which == 'tests':
        projects += glob.glob(r"unittests\**\*.dproj")
    if not which or which == 'samples':
        projects += glob.glob(r"samples\**\*.dproj")
    return projects


def build_delphi_project(ctx: context.Context, project_filename, config='DEBUG', delphi_version=DEFAULT_DELPHI_VERSION):
    delphi_versions = {
        "10.1": {"path": "18.0", "desc":"Delphi 10.1 Seattle"},
        "10.2": {"path": "19.0", "desc":"Delphi 10.2 Tokyo"},
        "10.3": {"path": "20.0", "desc":"Delphi 10.3 Rio"},
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
    cmdline = "7z a " + g_output_folder + f"\\..\\dmvcframework_{version}_samples.zip -r -i@7ziplistfile.txt"
    return subprocess.call(cmdline, shell=True) == 0


def create_zip(ctx, version):
    global g_output_folder
    print("CREATING ZIP")
    archive_name = r"..\dmvcframework_" + version + ".zip"
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
    # copying main sources
    print("Copying DMVCFramework Sources...")
    src = glob.glob("sources\\*.pas") + glob.glob("sources\\*.inc")
    for file in src:
        print("Copying " + file + " to " + g_output_folder + "\\sources")
        copy2(file, g_output_folder + "\\sources\\")

    # copying ideexperts
    print("Copying DMVCFramework IDEExpert...")
    src = glob.glob("ideexpert\\*.pas") + \
          glob.glob("ideexpert\\*.dfm") + \
          glob.glob("ideexpert\\*.ico") + \
          glob.glob("ideexpert\\*.bmp")

    for file in src:
        print("Copying " + file + " to " + g_output_folder + "\\ideexpert")
        copy2(file, g_output_folder + "\\ideexpert\\")

    # copying packages
    print("Copying DMVCFramework Delphi 10.1 Seattle packages...")
    os.makedirs(g_output_folder + "\\packages\\d101", exist_ok=True)
    copy2(r"packages\d101\dmvcframeworkRT.dpk", g_output_folder + "\\packages\\d102")
    copy2(r"packages\d101\dmvcframeworkRT.dproj", g_output_folder + "\\packages\\d102")

    # copying packages
    print("Copying DMVCFramework Delphi 10.2 Tokyo packages...")
    os.makedirs(g_output_folder + "\\packages\\d102", exist_ok=True)
    copy2(r"packages\d102\dmvcframeworkRT.dpk", g_output_folder + "\\packages\\d102")
    copy2(r"packages\d102\dmvcframeworkRT.dproj", g_output_folder + "\\packages\\d102")

    print("Copying DMVCFramework Delphi 10.3 Rio packages...")
    os.makedirs(g_output_folder + "\\packages\\d103", exist_ok=True)
    copy2(r"packages\d103\dmvcframeworkRT.dpk", g_output_folder + "\\packages\\d103")
    copy2(r"packages\d103\dmvcframeworkRT.dproj", g_output_folder + "\\packages\\d103")


def copy_libs():
    global g_output_folder

    # loggerpro
    print("Copying libraries: LoggerPro...")
    curr_folder = g_output_folder + "\\lib\\loggerpro"
    os.makedirs(curr_folder, exist_ok=True)
    src = glob.glob("lib\\loggerpro\\*.pas")
    for file in src:
        print("Copying " + file + " to " + curr_folder)
        copy2(file, curr_folder)
    copy2("lib\\loggerpro\\License.txt", curr_folder)
    copy2("lib\\loggerpro\\VERSION.TXT", curr_folder)

    # dmustache
    print("Copying libraries: dmustache...")
    curr_folder = g_output_folder + "\\lib\\dmustache"
    os.makedirs(curr_folder, exist_ok=True)
    src = glob.glob("lib\\dmustache\\*.pas") + \
          glob.glob("lib\\dmustache\\*.inc")
    for file in src:
        print("Copying " + file + " to " + curr_folder)
        copy2(file, curr_folder)
    copy2("lib\\dmustache\\README.md", curr_folder)


def printkv(key, value):
    print(Fore.RESET + (key + ': ').ljust(50) + Fore.GREEN + value + Fore.RESET)


def init_build(version):
    """Required by all tasks"""
    global g_version
    global g_output_folder
    global g_releases_path
    g_version = version
    g_output_folder = g_releases_path + "\\" + g_version
    printkv("BUILD VERSION", g_version)
    printkv('Output path', g_output_folder)
    rmtree(g_output_folder, True)
    os.makedirs(g_output_folder, exist_ok=True)
    f = open(g_output_folder + "\\version.txt", "w")
    f.write("VERSION " + g_version + "\n")
    f.write("BUILD DATETIME " + datetime.now().isoformat() + "\n")
    f.close()
    copy2("README.md", g_output_folder)
    copy2("3_0_0_breaking_changes.md", g_output_folder)
    copy2("3_1_0_breaking_changes.md", g_output_folder)
    copy2("License.txt", g_output_folder)


def build_delphi_project_list(ctx, projects, config="DEBUG", filter='', delphi_version=DEFAULT_DELPHI_VERSION):
    ret = True
    for delphi_project in projects:
        if filter and (not filter in delphi_project):
            print(f"Skipped {os.path.basename(delphi_project)}")
            continue
        msg = f"Building: {os.path.basename(delphi_project)}  ({config})"
        print(Fore.RESET + msg.ljust(70, '.'), end="")
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


@task(pre=[tests])
def release(ctx, version="DEBUG", delphi_version=DEFAULT_DELPHI_VERSION, skip_build=False):
    """Builds all the projects, executes integration tests and prepare the release"""
    init_build(version)
    if not skip_build:
        delphi_projects = get_delphi_projects_to_build('', delphi_version)
        build_delphi_project_list(ctx, delphi_projects, version, '', delphi_version)
    copy_sources()
    copy_libs()
    zip_samples(version)
    create_zip(ctx, version)


@task
def build_samples(ctx, version="DEBUG", filter="", delphi_version=DEFAULT_DELPHI_VERSION):
    """Builds samples"""
    init_build(version)
    delphi_projects = get_delphi_projects_to_build('samples', delphi_version)
    build_delphi_project_list(ctx, delphi_projects, version, filter, delphi_version)


@task
def build_core(ctx, version="DEBUG", delphi_version=DEFAULT_DELPHI_VERSION):
    """Builds core packages extensions"""
    init_build(version)
    delphi_projects = get_delphi_projects_to_build('core', delphi_version)
    build_delphi_project_list(ctx, delphi_projects, version, '', delphi_version)
