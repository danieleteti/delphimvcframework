from invoke import task, context, Exit
import os
import subprocess
from colorama import *
import glob
import shutil
from shutil import copy2, rmtree, copytree
from datetime import datetime
import pathlib
from typing import *
import time
from pathlib import Path

init()


class BuildConfig:
    """Configuration for the build process"""
    def __init__(self):
        self.releases_path = "releases"
        self.output = "bin"
        self.output_folder = ""  # defined at runtime
        self.version = "DEV"
        # Project root directory (where tasks.py is located)
        self.project_root = os.path.dirname(os.path.abspath(__file__))
        self.seven_zip = os.path.join(self.project_root, "7z.exe")

    @property
    def output_folder_path(self):
        return os.path.join(self.releases_path, self.version)


# Global config instance
config = BuildConfig()


delphi_versions = [
    {"version": "10.0", "path": "17.0", "desc": "Delphi 10 Seattle"},
    {"version": "10.1", "path": "18.0", "desc": "Delphi 10.1 Berlin"},
    {"version": "10.2", "path": "19.0", "desc": "Delphi 10.2 Tokyo"},
    {"version": "10.3", "path": "20.0", "desc": "Delphi 10.3 Rio"},
    {"version": "10.4", "path": "21.0", "desc": "Delphi 10.4 Sydney"},
    {"version": "11.0", "path": "22.0", "desc": "Delphi 11 Alexandria"},
    {"version": "11.1", "path": "22.0", "desc": "Delphi 11.1 Alexandria"},
    {"version": "11.2", "path": "22.0", "desc": "Delphi 11.2 Alexandria"},
    {"version": "11.3", "path": "22.0", "desc": "Delphi 11.3 Alexandria"},
    {"version": "12.0", "path": "23.0", "desc": "Delphi 12 Athens"},
    {"version": "13.0", "path": "37.0", "desc": "Delphi 13 Florence"},
]


def get_package_folders():
    """Get list of package folders by scanning the packages directory.
    Returns folders that match the pattern 'd*' (e.g., d100, d110, d130)"""
    packages_dir = "packages"
    if not os.path.isdir(packages_dir):
        return []
    folders = []
    for item in os.listdir(packages_dir):
        item_path = os.path.join(packages_dir, item)
        if os.path.isdir(item_path) and item.startswith("d") and item[1:].isdigit():
            folders.append(item)
    return sorted(folders)


def get_delphi_projects_to_build(which=""):
    projects = []
    delphi_version, _ = get_best_delphi_version_available()
    dversion = "d" + delphi_version["version"].replace(".", "")
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


def get_best_delphi_version_available() -> tuple[dict, str]:
    global delphi_version
    found = False
    rsvars_path = None
    i = len(delphi_versions)
    while (not found) and (i >= 0):
        i -= 1
        delphi_version = delphi_versions[i]
        version_path = delphi_version["path"]
        rsvars_path = f"C:\\Program Files (x86)\\Embarcadero\\Studio\\{version_path}\\bin\\rsvars.bat"
        if os.path.isfile(rsvars_path):
            found = True
        else:
            rsvars_path = f"D:\\Program Files (x86)\\Embarcadero\\Studio\\{version_path}\\bin\\rsvars.bat"
            if os.path.isfile(rsvars_path):
                found = True
    if found:
        return delphi_version, rsvars_path
    else:
        raise Exception("Cannot find a Delphi compiler")


def build_delphi_project(
    ctx: context.Context, project_filename, config="DEBUG", platform="Win32"
):
    delphi_version, rsvars_path = get_best_delphi_version_available()
    print("\nBUILD WITH: " + delphi_version["desc"])
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
    r = ctx.run(cmdline, hide=True, warn=True)
    if r.failed:
        print(r.stdout)
        print(r.stderr)
        raise Exit("Build failed for " + delphi_version["desc"])

def zip_samples(ctx, version):
    cmdline = (
        f'"{config.seven_zip}" a '
        + config.output_folder
        + f"\\..\\{version}_samples.zip -r -i@7ziplistfile.txt"
    )
    print("ZIPPING SAMPLES")
    print("CMDLINE: " + cmdline)
    result = ctx.run(cmdline, warn=True)
    if result.failed:
        print(Fore.RED + "ERROR: Failed to zip samples" + Fore.RESET)
        return False
    return True


def create_zip(ctx, version):
    print("CREATING ZIP")
    archive_name = "..\\" + version + ".zip"
    cmdline = f'"{config.seven_zip}" a {archive_name} *'
    print(cmdline)
    with ctx.cd(config.output_folder):
        result = ctx.run(cmdline, hide=False, warn=True)
        if result.failed:
            print(Fore.RED + "ERROR: Failed to create zip" + Fore.RESET)
            raise Exit("Failed to create zip archive")


def copy_sources():
    # Validate source directories exist
    ensure_dir_exists("sources", "DMVCFramework sources")
    ensure_dir_exists("ideexpert", "IDE Expert")
    ensure_dir_exists("packages", "Packages")
    ensure_dir_exists("tools\\entitygenerator", "Entity Generator tool")
    ensure_dir_exists("tools\\certificatesgenerator", "Certificates Generator tool")
    ensure_dir_exists("tools\\sample_env_file", "Sample env file tool")

    os.makedirs(config.output_folder + "\\sources", exist_ok=True)
    os.makedirs(config.output_folder + "\\ideexpert", exist_ok=True)
    os.makedirs(config.output_folder + "\\packages", exist_ok=True)
    os.makedirs(config.output_folder + "\\tools", exist_ok=True)
    # copying main sources
    print("Copying DMVCFramework Sources...")
    src = glob.glob("sources\\*.pas") + glob.glob("sources\\*.inc")
    for file in src:
        print("Copying " + file + " to " + config.output_folder + "\\sources")
        copy2(file, config.output_folder + "\\sources\\")

    # copying tools
    print("Copying tools...")
    ignore_patterns = shutil.ignore_patterns("*.identcache", "*.dcu", "__history", "__recovery")
    copytree("tools\\entitygenerator", config.output_folder + "\\tools\\entitygenerator", ignore=ignore_patterns)
    copytree(
        "tools\\certificatesgenerator",
        config.output_folder + "\\tools\\certificatesgenerator",
        ignore=ignore_patterns,
    )
    copytree("tools\\sample_env_file", config.output_folder + "\\tools\\sample_env_file", ignore=ignore_patterns)

    # copying ideexperts
    print("Copying DMVCFramework IDEExpert...")
    src = (
        glob.glob("ideexpert\\*.pas")
        + glob.glob("ideexpert\\*.dfm")
        + glob.glob("ideexpert\\*.ico")
        + glob.glob("ideexpert\\*.bmp")
        + glob.glob("ideexpert\\*.png")
        + glob.glob("ideexpert\\*.res")
    )

    for file in src:
        print("Copying " + file + " to " + config.output_folder + "\\ideexpert")
        copy2(file, config.output_folder + "\\ideexpert\\")

    files = [
        #"dmvcframeworkDTResource.rc",
        "dmvcframework_group.groupproj",
        "dmvcframeworkRT.dproj",
        "dmvcframeworkRT.dpk",
        "dmvcframeworkDT.dproj",
        "dmvcframeworkDT.dpk",
        # loggerproRT è in lib\loggerpro\packages\
        # SwagDoc è in lib\swagdoc\
    ]

    # Get package folders dynamically from packages directory
    folders = get_package_folders()
    if not folders:
        raise Exit("No package folders found in packages directory")

    for folder in folders:
        print(f"Copying DMVCFramework Delphi {folder} packages...")
        for file in files:
            os.makedirs(config.output_folder + f"\\packages\\{folder}", exist_ok=True)
            print("Copying " + file + " to " + config.output_folder + f"\\packages\\{folder}")
            copy2(
                rf"packages\{folder}\{file}", config.output_folder + rf"\packages\{folder}"
            )


def ensure_dir_exists(path, description=""):
    """Validate that a directory exists, raise Exit if not"""
    if not os.path.isdir(path):
        desc = f" ({description})" if description else ""
        raise Exit(f"Source directory not found{desc}: {path}")


def run_robocopy(ctx, source, dest, extra_args=""):
    """Run robocopy and handle its non-standard exit codes.
    Robocopy exit codes: 0-7 = success, 8+ = error"""
    ensure_dir_exists(source)
    # Always exclude Delphi compilation artifacts
    default_excludes = "/XF *.identcache *.dcu"
    cmd = rf"robocopy {source} {dest} /E /NFL /NDL /NJH /NJS /nc /ns /np /r:1 /w:1 {default_excludes} {extra_args}"
    result = ctx.run(cmd, warn=True, hide=True)
    # Robocopy: exit codes 0-7 are success, 8+ are errors
    if result.return_code >= 8:
        print(Fore.RED + f"ERROR: robocopy failed with exit code {result.return_code}" + Fore.RESET)
        print(f"Command: {cmd}")
        print(f"stdout: {result.stdout}")
        print(f"stderr: {result.stderr}")
        raise Exit(f"Cannot copy from {source} to {dest}")
    return True


def copy_libs(ctx):
    # swagdoc
    print("Copying libraries: SwagDoc...")
    curr_folder = config.output_folder + "\\lib\\swagdoc"
    os.makedirs(curr_folder, exist_ok=True)
    run_robocopy(ctx, r"lib\swagdoc", curr_folder)

    # loggerpro
    print("Copying libraries: LoggerPro...")
    curr_folder = config.output_folder + "\\lib\\loggerpro"
    os.makedirs(curr_folder, exist_ok=True)
    run_robocopy(ctx, r"lib\loggerpro", curr_folder,
                 "/XD .vscode __history __recovery samples unittests "
                 "/XF *.log *.png *.ico LOGGERPRO-BUILD-TIMESTAMP.TXT")

    # dmustache
    print("Copying libraries: dmustache...")
    curr_folder = config.output_folder + "\\lib\\dmustache"
    os.makedirs(curr_folder, exist_ok=True)
    run_robocopy(ctx, r"lib\dmustache", curr_folder, "/XF *.log *.png *.ico")


def printkv(key, value):
    print(Fore.RESET + key + ": " + Fore.GREEN + value.rjust(60) + Fore.RESET)


def init_build(version, clean_releases=False):
    """Required by all tasks"""
    config.version = version
    config.output_folder = config.releases_path + "\\" + config.version
    print()
    print(Fore.RESET + Fore.RED + "*" * 80)
    print(Fore.RESET + Fore.RED + " BUILD VERSION: " + config.version + Fore.RESET)
    print(Fore.RESET + Fore.RED + " OUTPUT PATH  : " + config.output_folder + Fore.RESET)
    print(Fore.RESET + Fore.RED + "*" * 80)

    if clean_releases:
        print("Cleaning releases folder...")
        rmtree(config.releases_path, True)
    else:
        rmtree(config.output_folder, True)
    os.makedirs(config.output_folder, exist_ok=True)
    f = open(config.output_folder + "\\version.txt", "w")
    f.write("VERSION " + config.version + "\n")
    f.write("BUILD DATETIME " + datetime.now().isoformat() + "\n")
    f.close()
    copy2("README.md", config.output_folder)
    copy2("License.txt", config.output_folder)


def build_delphi_project_list(ctx, projects, build_config="DEBUG", filter=""):
    ret = True
    for delphi_project in projects:
        if filter and (not filter in delphi_project):
            print(f"Skipped {os.path.basename(delphi_project)}")
            continue
        msg = f"Building: {os.path.basename(delphi_project)}  ({build_config})"
        print(Fore.RESET + msg.ljust(90, "."), end="")
        try:
            build_delphi_project(ctx, delphi_project, build_config)
            print(Fore.GREEN + "OK" + Fore.RESET)
        except Exception as e:
            print(Fore.RED + "\n\nBUILD ERROR")
            print(Fore.RESET)
            print(e)

    return ret


@task
def clean(ctx, folder=None):
    if folder is None:
        folder = config.output_folder
    if not folder:
        raise Exit("No folder specified for clean operation")
    if not os.path.isdir(folder):
        print(f"Folder does not exist, nothing to clean: {folder}")
        return
    print(f"Cleaning folder {folder}")
    # Files to preserve (source resources, not compiled)
    preserve_files = {"DMVC.Splash.Resources.res"}

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

    # Filter out preserved files
    to_delete = [f for f in to_delete if os.path.basename(f) not in preserve_files]

    for f in to_delete:
        print(f"Deleting {f}")
        os.remove(f)

    rmtree(folder + r"\lib\loggerpro\Win32", True)
    # Clean loggerpro packages - find all package folders dynamically
    loggerpro_packages = folder + r"\lib\loggerpro\packages"
    if os.path.isdir(loggerpro_packages):
        for pkg_folder in os.listdir(loggerpro_packages):
            pkg_path = os.path.join(loggerpro_packages, pkg_folder)
            if os.path.isdir(pkg_path):
                rmtree(os.path.join(pkg_path, "__history"), True)
                rmtree(os.path.join(pkg_path, "Win32", "Debug"), True)
                rmtree(os.path.join(pkg_path, "Win64", "Debug"), True)
    rmtree(folder + r"\lib\dmustache\.git", True)
    rmtree(folder + r"\lib\swagdoc\lib", True)
    rmtree(folder + r"\lib\swagdoc\deploy", True)
    rmtree(folder + r"\lib\swagdoc\demos", True)


def _run_tests(ctx, platform):
    """Internal function to build and execute unit tests for a specific platform"""
    bin_folder = "bin32" if platform == "Win32" else "bin64"
    testclient = r"unittests\general\TestClient\DMVCFrameworkTests.dproj"
    testserver = r"unittests\general\TestServer\TestServer.dproj"

    print(f"\n{'='*60}")
    print(f"Running {platform} tests")
    print(f"{'='*60}")

    print("\nBuilding Unit Test client")
    build_delphi_project(ctx, testclient, config="CI", platform=platform)
    print("\nBuilding Test Server")
    build_delphi_project(ctx, testserver, config="CI", platform=platform)

    print("\nExecuting tests...")
    server_proc = subprocess.Popen(
        [r"unittests\general\TestServer\bin\TestServer.exe"],
        shell=True
    )
    time.sleep(1)
    r = None
    try:
        r = subprocess.run(
            [rf"unittests\general\TestClient\{bin_folder}\DMVCFrameworkTests.exe"]
        )
        if r.returncode != 0:
            raise Exit(f"Cannot run unit test client ({platform}): \n" + str(r.stdout))
    finally:
        subprocess.run(["taskkill", "/f", "/im", "TestServer.exe"],
                      capture_output=True)
    if r.returncode > 0:
        print(r)
        print(f"Unit Tests Failed ({platform})")
        raise Exit(f"Unit tests failed ({platform})")


@task()
def tests32(ctx):
    """Builds and execute the unit tests (Win32)"""
    _run_tests(ctx, "Win32")


@task()
def tests64(ctx):
    """Builds and execute the unit tests (Win64)"""
    _run_tests(ctx, "Win64")


@task(pre=[tests32, tests64])
def tests(ctx):
    """Builds and execute all unit tests (Win32 and Win64)"""
    pass


def get_version_from_file():
    with open(r".\sources\dmvcframeworkbuildconsts.inc") as f:
        lines = f.readlines()
    res = [x for x in lines if "DMVCFRAMEWORK_VERSION" in x]
    if len(res) != 1:
        raise Exception(
            "Cannot find DMVCFRAMEWORK_VERSION in dmvcframeworkbuildconsts.inc file"
        )
    version_line: str = res[0]
    version_line = version_line.strip(" ;\t")
    pieces = version_line.split("=")
    if len(pieces) != 2:
        raise Exception(
            "Version line in wrong format in dmvcframeworkbuildconsts.inc file: "
            + version_line
        )
    version = pieces[1].strip("' ")
    if not "framework" in version:
        version = "dmvcframework-" + version
    if "beta" in version.lower():
        print(Fore.RESET + Fore.RED + "WARNING - BETA VERSION: " + version + Fore.RESET)
    else:
        print(Fore.RESET + Fore.GREEN + "BUILDING VERSION: " + version + Fore.RESET)
    return version


@task()
def release(
    ctx,
    skip_build=False,
    skip_tests=False,
):
    """Builds all the projects, executes integration tests and prepare the release"""

    version = get_version_from_file()

    init_build(version, clean_releases=True)

    if not skip_tests:
        tests32(ctx)
        tests64(ctx)
    if not skip_build:
        delphi_projects = get_delphi_projects_to_build("")
        if not _build_projects(ctx, delphi_projects, version, ""):
            return False
    print(Fore.RESET)
    copy_sources()
    copy_libs(ctx)
    clean(ctx)
    zip_samples(ctx, version)
    create_zip(ctx, version)
    return True


def _build_projects(ctx, delphi_projects, version, filter):
    return build_delphi_project_list(ctx, delphi_projects, version, filter)


@task
def build_samples(ctx, version="DEBUG", filter=""):
    """Builds samples"""
    init_build(version)
    delphi_projects = get_delphi_projects_to_build("samples")
    return _build_projects(ctx, delphi_projects, version, filter)


@task(post=[])
def build_core(ctx, version="DEBUG"):
    """Builds core packages extensions"""
    init_build(version)
    delphi_projects = get_delphi_projects_to_build("core")
    if not _build_projects(ctx, delphi_projects, version, ""):
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
        [
            "AnsiString",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (LeftValue.Value = RightValue.Value))",
        ],
        [
            "String",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (LeftValue.Value = RightValue.Value))",
        ],
        [
            "Currency",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (LeftValue.Value = RightValue.Value))",
        ],
        [
            "Boolean",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (LeftValue.Value = RightValue.Value))",
        ],
        [
            "TDate",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (DateAreEquals(LeftValue.Value, RightValue.Value)))",
        ],
        [
            "TTime",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (TimeAreEquals(LeftValue.Value, RightValue.Value)))",
        ],
        [
            "TDateTime",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t (DateAreEquals(LeftValue.Value, RightValue.Value) and \n\t TimeAreEquals(LeftValue.Value, RightValue.Value)))",
        ],
        [
            "Single",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t SameValue(LeftValue.Value, RightValue.Value, 0.000001))",
        ],
        [
            "Double",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t SameValue(LeftValue.Value, RightValue.Value, 0.000000001))",
        ],
        [
            "Float32",  # like Single
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t SameValue(LeftValue.Value, RightValue.Value, 0.000001))",
        ],
        [
            "Float64",  # like Double
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t SameValue(LeftValue.Value, RightValue.Value, 0.000000001))",
        ],
        [
            "Extended",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and \n\t SameValue(LeftValue.Value, RightValue.Value, 0.000000001))",
        ],
        [
            "Int8",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "UInt8",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "Byte",  # like UInt8
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "Int16",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "UInt16",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "Int32",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "Integer",  # like Int32
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "UInt32",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "Int64",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "UInt64",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "TGUID",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "NativeInt",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
        [
            "NativeUInt",
            "(LeftValue.IsNull and RightValue.IsNull) or ((LeftValue.HasValue and RightValue.HasValue) and (LeftValue.Value = RightValue.Value))",
        ],
    ]

    str_main_tmpl = "".join(main_tmpl)
    str_intf_tmpl = "".join(intf_tmpl)
    str_impl_tmpl = "".join(impl_tmpl)

    intf_out = ""
    impl_out = ""

    enum_declaration = ["ntInvalidNullableType"]
    enum_detect_line = []
    for delphi_type, type_compare in delphi_types:
        enum_declaration.append("ntNullable" + delphi_type)
        enum_detect_line.append(
            f"  if aTypeInfo = TypeInfo(Nullable{delphi_type}) then \n    Exit(ntNullable{delphi_type}); "
        )

        intf_out += (
            f"//**************************\n// ** Nullable{delphi_type}\n//**************************\n\n"
            + str_intf_tmpl.replace("$TYPE$", delphi_type)
        )
        impl_out += (
            str_impl_tmpl.replace("$TYPE$", delphi_type).replace(
                "$COMPARE$", type_compare
            )
            + "\n"
        )

    enum_declaration = (
        "  TNullableType = (\n     " + "\n   , ".join(enum_declaration) + ");\n\n"
    )
    enum_detect_function = []
    enum_detect_function.append(
        "function GetNullableType(const aTypeInfo: PTypeInfo): TNullableType;"
    )
    enum_detect_function.append("begin")
    enum_detect_function.extend(enum_detect_line)
    enum_detect_function.append("  Result := ntInvalidNullableType;")
    enum_detect_function.append("end;")

    intf_out += enum_declaration + "\n"
    intf_out += enum_detect_function[0] + "\n"
    impl_out += "\n".join(enum_detect_function) + "\n"

    str_main_tmpl = (
        str_main_tmpl.replace("$INTERFACE$", intf_out).replace(
            "$IMPLEMENTATION$", impl_out
        )
        + "\n"
    )

    with open(output_unitname, "w") as f:
        f.writelines(str_main_tmpl)
    print(f"Generated: {output_unitname}")
