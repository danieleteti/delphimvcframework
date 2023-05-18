from invoke import task, context, Exit
import os
import subprocess
from colorama import *
import glob
from shutil import copy2, rmtree, copytree
from datetime import datetime
import pathlib
from typing import *

from pathlib import Path

init()

DEFAULT_DELPHI_VERSION = "11.3"

g_releases_path = "releases"
g_output = "bin"
g_output_folder = ""  # defined at runtime
g_version = "DEV"


def get_delphi_projects_to_build(which="", delphi_version=DEFAULT_DELPHI_VERSION):
    projects = []
    dversion = "d" + delphi_version.replace(".", "")
    if not which or which == "core":
        projects += glob.glob(
            r"packages\{dversion}\*.groupproj".format(dversion=dversion)
        )
        projects += glob.glob(r"tools\entitygenerator\MVCAREntitiesGenerator.dproj")
    if not which or which == "tests":
        projects += glob.glob(r"unittests\**\*.dproj")
    if not which or which == "samples":
        projects += glob.glob(r"samples\**\*.dproj")
        projects += glob.glob(r"samples\**\**\*.dproj")
        projects += glob.glob(r"samples\**\**\**\*.dproj")
    return sorted(projects)


def build_delphi_project(
    ctx: context.Context,
    project_filename,
    config="DEBUG",
    delphi_version=DEFAULT_DELPHI_VERSION,
    platform="Win32"
):
    delphi_versions = {
        "10": {"path": "17.0", "desc": "Delphi 10 Seattle"},
        "10.1": {"path": "18.0", "desc": "Delphi 10.1 Berlin"},
        "10.2": {"path": "19.0", "desc": "Delphi 10.2 Tokyo"},
        "10.3": {"path": "20.0", "desc": "Delphi 10.3 Rio"},
        "10.4": {"path": "21.0", "desc": "Delphi 10.4 Sydney"},
        "11": {"path": "22.0", "desc": "Delphi 11 Alexandria"},
        "11.1": {"path": "22.0", "desc": "Delphi 11.1 Alexandria"},
        "11.2": {"path": "22.0", "desc": "Delphi 11.2 Alexandria"},
        "11.3": {"path": "22.0", "desc": "Delphi 11.3 Alexandria"},
    }

    assert delphi_version in delphi_versions, (
        "Invalid Delphi version: " + delphi_version
    )
    print("[" + delphi_versions[delphi_version]["desc"] + "] ", end="")
    version_path = delphi_versions[delphi_version]["path"]

    rsvars_path = (
        f"C:\\Program Files (x86)\\Embarcadero\\Studio\\{version_path}\\bin\\rsvars.bat"
    )
    if not os.path.isfile(rsvars_path):
        rsvars_path = f"D:\\Program Files (x86)\\Embarcadero\\Studio\\{version_path}\\bin\\rsvars.bat"
        if not os.path.isfile(rsvars_path):
            raise Exception("Cannot find rsvars.bat")
    cmdline = (
        '"'
        + rsvars_path
        + '"'
        + " & msbuild /t:Build /p:Config="
        + config
        + f' /p:Platform={platform} "'
        + project_filename
        + '"'
    )
    print("\n" + "".join(cmdline))
    r = ctx.run(cmdline, hide=True, warn=True)
    if r.failed:
        print(r.stdout)
        print(r.stderr)
        raise Exit("Build failed for " + delphi_versions[delphi_version]["desc"])


def zip_samples(version):
    global g_output_folder
    cmdline = (
        "7z a "
        + g_output_folder
        + f"\\..\\{version}_samples.zip -r -i@7ziplistfile.txt"
    )
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
    copytree("tools\\entitygenerator", g_output_folder + "\\tools\\entitygenerator")
    # copytree('tools\\rql2sql', g_output_folder + "\\tools\\rql2sql")

    # copying ideexperts
    print("Copying DMVCFramework IDEExpert...")
    src = (
        glob.glob("ideexpert\\*.pas")
        + glob.glob("ideexpert\\*.dfm")
        + glob.glob("ideexpert\\*.ico")
        + glob.glob("ideexpert\\*.bmp")
    )

    for file in src:
        print("Copying " + file + " to " + g_output_folder + "\\ideexpert")
        copy2(file, g_output_folder + "\\ideexpert\\")

    files = [
        "dmvcframeworkDTResource.rc",
        "dmvcframework_group.groupproj",
        "dmvcframeworkRT.dproj",
        "dmvcframeworkRT.dpk",
        "dmvcframeworkDT.dproj",
        "dmvcframeworkDT.dpk",
    ]

    folders = ["d100", "d101", "d102", "d103", "d104","d110","d111","d112","d113"]

    for folder in folders:
        print(f"Copying DMVCFramework Delphi {folder} packages...")
        for file in files:
            os.makedirs(g_output_folder + f"\\packages\\{folder}", exist_ok=True)
            copy2(
                rf"packages\{folder}\{file}", g_output_folder + rf"\packages\{folder}"
            )
    # copy2(
    #     rf"packages\common_contains.inc", g_output_folder + rf"\packages"
    # )
    # copy2(
    #     rf"packages\common_defines.inc", g_output_folder + rf"\packages"
    # )
    # copy2(
    #     rf"packages\common_defines_design.inc", g_output_folder + rf"\packages"
    # )


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


def printkv(key, value):
    print(Fore.RESET + key + ": " + Fore.GREEN + RightValue.rjust(60) + Fore.RESET)


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


def build_delphi_project_list(
    ctx, projects, config="DEBUG", filter="", delphi_version=DEFAULT_DELPHI_VERSION
):
    ret = True
    for delphi_project in projects:
        if filter and (not filter in delphi_project):
            print(f"Skipped {os.path.basename(delphi_project)}")
            continue
        msg = f"Building: {os.path.basename(delphi_project)}  ({config})"
        print(Fore.RESET + msg.ljust(90, "."), end="")
        try:
            build_delphi_project(ctx, delphi_project, "DEBUG", delphi_version)
            print(Fore.GREEN + "OK" + Fore.RESET)
        except Exception as e:
            print(Fore.RED + "\n\nBUILD ERROR")
            print(Fore.RESET)
            print(e)

        # if res.ok:
        #     print(Fore.GREEN + "OK" + Fore.RESET)
        # else:
        #     ret = False
        #     print(Fore.RED + "\n\nBUILD ERROR")
        #     print(Fore.RESET + res.stdout)
        #     print("\n")

    return ret


@task
def clean(ctx, folder=None):
    global g_output_folder
    import os
    import glob

    if folder is None:
        folder = g_output_folder
    print(f"Cleaning folder {folder}")
    output = pathlib.Path(folder)
    to_delete = []
    to_delete += glob.glob(folder + r"\**\*.exe", recursive=True)
    to_delete += glob.glob(folder + r"\**\*.dcu", recursive=True)
    to_delete += glob.glob(folder + r"\**\*.stat", recursive=True)
    to_delete += glob.glob(folder + r"\**\*.res", recursive=True)
    to_delete += glob.glob(folder + r"\**\*.map", recursive=True)
    to_delete += glob.glob(folder + r"\**\*.~*", recursive=True)
    to_delete += glob.glob(folder + r"\**\*.rsm", recursive=True)
    to_delete += glob.glob(folder + r"\**\*.drc", recursive=True)
    to_delete += glob.glob(folder + r"\**\*.log", recursive=True)
    to_delete += glob.glob(folder + r"\**\*.local", recursive=True)
    to_delete += glob.glob(folder + r"\**\*.gitignore", recursive=True)
    to_delete += glob.glob(folder + r"\**\*.gitattributes", recursive=True)

    for f in to_delete:
        print(f"Deleting {f}")
        os.remove(f)

    rmtree(folder + r"\lib\loggerpro\Win32", True)
    rmtree(folder + r"\lib\loggerpro\packages\d100\__history", True)
    rmtree(folder + r"\lib\loggerpro\packages\d100\Win32\Debug", True)
    rmtree(folder + r"\lib\loggerpro\packages\d101\__history", True)
    rmtree(folder + r"\lib\loggerpro\packages\d101\Win32\Debug", True)
    rmtree(folder + r"\lib\loggerpro\packages\d102\__history", True)
    rmtree(folder + r"\lib\loggerpro\packages\d102\Win32\Debug", True)
    rmtree(folder + r"\lib\loggerpro\packages\d103\__history", True)
    rmtree(folder + r"\lib\loggerpro\packages\d103\Win32\Debug", True)
    rmtree(folder + r"\lib\loggerpro\packages\d104\__history", True)
    rmtree(folder + r"\lib\loggerpro\packages\d104\Win32\Debug", True)
    rmtree(folder + r"\lib\loggerpro\packages\d110\__history", True)
    rmtree(folder + r"\lib\loggerpro\packages\d110\Win32\Debug", True)
    rmtree(folder + r"\lib\loggerpro\packages\d111\__history", True)
    rmtree(folder + r"\lib\loggerpro\packages\d111\Win32\Debug", True)
    rmtree(folder + r"\lib\loggerpro\packages\d112\__history", True)
    rmtree(folder + r"\lib\loggerpro\packages\d112\Win32\Debug", True)
    rmtree(folder + r"\lib\dmustache\.git", True)
    rmtree(folder + r"\lib\swagdoc\lib", True)
    rmtree(folder + r"\lib\swagdoc\deploy", True)
    rmtree(folder + r"\lib\swagdoc\demos", True)


@task()
def tests32(ctx, delphi_version=DEFAULT_DELPHI_VERSION):
    """Builds and execute the unit tests"""
    import os

    apppath = os.path.dirname(os.path.realpath(__file__))
    res = True
    testclient = r"unittests\general\Several\DMVCFrameworkTests.dproj"
    testserver = r"unittests\general\TestServer\TestServer.dproj"

    print("\nBuilding Unit Test client")
    build_delphi_project(ctx, testclient, config="CI", delphi_version=delphi_version, platform="Win32")
    print("\nBuilding Test Server")
    build_delphi_project(ctx, testserver, config="CI", delphi_version=delphi_version, platform="Win32")

    # import subprocess
    # subprocess.run([r"unittests\general\TestServer\Win32\Debug\TestServer.exe"])
    # os.spawnl(os.P_NOWAIT, r"unittests\general\TestServer\Win32\Debug\TestServer.exe")
    import subprocess

    print("\nExecuting tests...")
    subprocess.Popen([r"unittests\general\TestServer\bin\TestServer.exe"], shell=True)
    r = subprocess.run([r"unittests\general\Several\bin32\DMVCFrameworkTests.exe"])
    if r.returncode != 0:
        return Exit("Compilation failed: \n" + str(r.stdout))
    subprocess.run(["taskkill", "/f", "/im", "TestServer.exe"])
    if r.returncode > 0:
        print(r)
        print("Unit Tests Failed")
        return Exit("Unit tests failed")


@task()
def tests64(ctx, delphi_version=DEFAULT_DELPHI_VERSION):
    """Builds and execute the unit tests"""
    import os

    apppath = os.path.dirname(os.path.realpath(__file__))
    res = True
    testclient = r"unittests\general\Several\DMVCFrameworkTests.dproj"
    testserver = r"unittests\general\TestServer\TestServer.dproj"

    print("\nBuilding Unit Test client")
    build_delphi_project(ctx, testclient, config="CI", delphi_version=delphi_version, platform="Win64")
    print("\nBuilding Test Server")
    build_delphi_project(ctx, testserver, config="CI", delphi_version=delphi_version, platform="Win64")

    # import subprocess
    # subprocess.run([r"unittests\general\TestServer\Win32\Debug\TestServer.exe"])
    # os.spawnl(os.P_NOWAIT, r"unittests\general\TestServer\Win32\Debug\TestServer.exe")
    import subprocess

    print("\nExecuting tests...")
    subprocess.Popen([r"unittests\general\TestServer\bin\TestServer.exe"], shell=True)
    r = subprocess.run([r"unittests\general\Several\bin64\DMVCFrameworkTests.exe"])
    if r.returncode != 0:
        return Exit("Compilation failed: \n" + str(r.stdout))
    subprocess.run(["taskkill", "/f", "/im", "TestServer.exe"])
    if r.returncode > 0:
        print(r)
        print("Unit Tests Failed")
        return Exit("Unit tests failed")


@task(pre=[tests32, tests64])
def tests(ctx, delphi_version=DEFAULT_DELPHI_VERSION):
    pass

@task()
def release(
    ctx, version="DEBUG", delphi_version=DEFAULT_DELPHI_VERSION, skip_build=False, skip_tests=False
):
    """Builds all the projects, executes integration tests and prepare the release"""
    init_build(version)

    if not skip_tests: tests(ctx, delphi_version)
    if not skip_build:
        delphi_projects = get_delphi_projects_to_build("", delphi_version)
        if not build_delphi_project_list(
            ctx, delphi_projects, version, "", delphi_version
        ):
            return False  # fails build
    print(Fore.RESET)
    copy_sources()
    copy_libs(ctx)
    clean(ctx)
    zip_samples(version)
    create_zip(ctx, version)


@task
def build_samples(
    ctx, version="DEBUG", filter="", delphi_version=DEFAULT_DELPHI_VERSION
):
    """Builds samples"""
    init_build(version)
    delphi_projects = get_delphi_projects_to_build("samples", delphi_version)
    return build_delphi_project_list(
        ctx, delphi_projects, version, filter, delphi_version
    )


@task(post=[])
def build_core(ctx, version="DEBUG", delphi_version=DEFAULT_DELPHI_VERSION):
    """Builds core packages extensions"""
    init_build(version)
    delphi_projects = get_delphi_projects_to_build("core", delphi_version)
    ret = build_delphi_project_list(ctx, delphi_projects, version, "", delphi_version)
    if not ret:
        raise Exit("Build failed")


def parse_template(tmpl: List[str]):
    main_tmpl = []
    intf_tmpl = []
    impl_tmpl = []

    state = "verbatim"
    for row in tmpl:
        if row.upper().strip() == "///INTERFACE.BEGIN":
            state = "parsing.interface"
            continue
        if row.upper().strip() == "///IMPLEMENTATION.BEGIN":
            state = "parsing.implementation"
            continue
        if row.upper().strip() in ["///INTERFACE.END", "///IMPLEMENTATION.END"]:
            if state == "parsing.interface":
                main_tmpl.append("$INTERFACE$")
            if state == "parsing.implementation":                
                main_tmpl.append("$IMPLEMENTATION$")
            state = "verbatim"
            continue

        if state == "parsing.interface":
            intf_tmpl.append(row)
        elif state == "parsing.implementation":
            impl_tmpl.append(row)
        elif state == "verbatim":
            main_tmpl.append(row)
    return main_tmpl, intf_tmpl, impl_tmpl


@task
def generate_nullables(ctx):
    import pathlib

    src_folder = pathlib.Path(__file__).parent.joinpath("sources")
    template_unitname = src_folder.joinpath("MVCFramework.Nullables.pas.template")
    output_unitname = src_folder.joinpath("MVCFramework.Nullables.pas")

    with open(template_unitname, "r") as f:
        rows = f.readlines()

    main_tmpl, intf_tmpl, impl_tmpl = parse_template(rows)

    delphi_types = [
        ["String", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (LeftValue.Value = RightValue.Value))"],
        ["Currency", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (LeftValue.Value = RightValue.Value))"],
        ["Boolean", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (LeftValue.Value = RightValue.Value))"],
        ["TDate", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (DateToISODate(LeftValue.Value) = DateToISODate(RightValue.Value)))"],
        ["TTime", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (TimeToISOTime(LeftValue.Value) = TimeToISOTime(RightValue.Value)))"],
        ["TDateTime", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (DateTimeToISOTimeStamp(LeftValue.Value) = DateTimeToISOTimeStamp(RightValue.Value)))"],
        ["Single", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t SameValue(LeftValue.Value, RightValue.Value, 0.000001))"],
        ["Double", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t SameValue(LeftValue.Value, RightValue.Value, 0.000000001))"],
        ["Extended", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t SameValue(LeftValue.Value, RightValue.Value, 0.000000001))"],
        ["Int16", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))"],
        ["UInt16", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))"],
        ["Int32", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))"],
        ["UInt32", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))"],
        ["Int64", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))"],
        ["UInt64", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))"],
        ["TGUID", "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))"],
    ]

    str_main_tmpl = "".join(main_tmpl)
    str_intf_tmpl = "".join(intf_tmpl)
    str_impl_tmpl = "".join(impl_tmpl)

    intf_out = ""
    impl_out = ""

    enum_declaration = ["ntInvalidNullableType"]
    enum_detect_line = []
    for delphi_type, type_compare in delphi_types:
        enum_declaration.append('ntNullable' + delphi_type)
        enum_detect_line.append(f"  if aTypeInfo = TypeInfo(Nullable{delphi_type}) then \n    Exit(ntNullable{delphi_type}); ")

        intf_out += (
            f"//**************************\n// ** Nullable{delphi_type}\n//**************************\n\n"
            + str_intf_tmpl.replace("$TYPE$", delphi_type)
        )
        impl_out += str_impl_tmpl \
            .replace("$TYPE$", delphi_type) \
            .replace("$COMPARE$", type_compare) + "\n"


    enum_declaration = '  TNullableType = (\n     ' + '\n   , '.join(enum_declaration) + ');\n\n' 
    enum_detect_function = []
    enum_detect_function.append("function GetNullableType(const aTypeInfo: PTypeInfo): TNullableType;")
    enum_detect_function.append("begin")
    enum_detect_function.extend(enum_detect_line)        
    enum_detect_function.append("  Result := ntInvalidNullableType;")
    enum_detect_function.append("end;")

    intf_out += enum_declaration + "\n"
    intf_out += enum_detect_function[0] + "\n"
    impl_out += "\n".join(enum_detect_function) + "\n"

    str_main_tmpl = str_main_tmpl \
        .replace("$INTERFACE$", intf_out) \
        .replace("$IMPLEMENTATION$", impl_out) \
        + "\n"

    with open(output_unitname, "w") as f:
        f.writelines(str_main_tmpl)

    with open(src_folder.joinpath("main.out.txt"), "w") as f:
        f.writelines(main_tmpl)

    with open(src_folder.joinpath("interface.out.txt"), "w") as f:
        f.writelines(intf_tmpl)

    with open(src_folder.joinpath("implementation.out.txt"), "w") as f:
        f.writelines(impl_tmpl)
