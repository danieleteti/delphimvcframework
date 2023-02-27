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

DEFAULT_DELPHI_VERSION = "11.1"

g_output = "bin"
g_output_folder = ""  # defined at runtime
g_version = "DEV"

projects = [
    ("samples\\01_global_logger\\global_logger.dproj", "Win32"),
    ("samples\\02_file_appender\\file_appender.dproj", "Win32"),
    ("samples\\03_console_appender\\console_appender.dproj", "Win32"),
    (
        "samples\\04_outputdebugstring_appender\\outputdebugstring_appender.dproj",
        "Win32",
    ),
    ("samples\\05_vcl_appenders\\vcl_appenders.dproj", "Win32"),
    ("samples\\08_email_appender\\email_appender.dproj", "Win32"),
    ("samples\\10_multiple_appenders\\multiple_appenders.dproj", "Win32"),
    (
        "samples\\15_appenders_with_different_log_levels\\multi_appenders_different_loglevels.dproj",
        "Win32",
    ),
    ("samples\\20_multiple_loggers\\multiple_loggers.dproj", "Win32"),
    ("samples\\50_custom_appender\\custom_appender.dproj", "Win32"),
    ("samples\\60_logging_inside_dll\\MainProgram.dproj", "Win32"),
    ("samples\\60_logging_inside_dll\\mydll.dproj", "Win32"),
    ("samples\\70_isapi_sample\\loggerproisapisample.dproj", "Win32"),
    ("samples\\90_remote_logging_with_redis\\REDISAppenderSample.dproj", "Win32"),
    (
        "samples\\90_remote_logging_with_redis\\redis_logs_viewer\\REDISLogsViewer.dproj",
        "Win32",
    ),
    ("samples\\100_udp_syslog\\udp_syslog.dproj", "Win32"),
    ("samples\\110_rest_appender\RESTAppenderSample.dproj", "Win32"),
    ("samples\\110_rest_appender_mobile\RESTAppenderMobileSample.dproj", "Android"),
    (
        "samples\\120_elastic_search_appender\\ElasticSearchAppenderSample.dproj",
        "Win32",
    ),
    ("samples\\rest_logs_collector\RESTLogsCollector.dproj", "Win32"),
]


def get_delphi_projects_to_build(delphi_version=DEFAULT_DELPHI_VERSION):
    global projects
    return projects


def build_delphi_project(
    ctx: context.Context,
    project_filename,
    config="DEBUG",
    delphi_version=DEFAULT_DELPHI_VERSION,
):
    delphi_versions = {
        "10": {"path": "17.0", "desc": "Delphi 10 Seattle"},
        "10.1": {"path": "18.0", "desc": "Delphi 10.1 Berlin"},
        "10.2": {"path": "19.0", "desc": "Delphi 10.2 Tokyo"},
        "10.3": {"path": "20.0", "desc": "Delphi 10.3 Rio"},
        "10.4": {"path": "21.0", "desc": "Delphi 10.4 Sydney"},
        "11": {"path": "22.0", "desc": "Delphi 11 Alexandria"},
        "11.1": {"path": "22.0", "desc": "Delphi 11.1 Alexandria"},
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
        + f' /p:Platform={project_filename[1]} "'
        + project_filename[0]
        + '"'
    )
    print("\n" + "".join(cmdline))
    r = ctx.run(cmdline, hide=True, warn=True)
    if r.failed:
        print(r.stdout)
        print(r.stderr)
        raise Exit("Build failed for " + delphi_versions[delphi_version]["desc"])


def build_delphi_project_list(
    ctx, projects, config="DEBUG", delphi_version=DEFAULT_DELPHI_VERSION
):
    ret = True
    for delphi_project in projects:
        msg = f"Building: {os.path.basename(delphi_project[0])}  ({config})"
        print(Fore.RESET + msg.ljust(90, "."), end="")
        try:
            build_delphi_project(ctx, delphi_project, "DEBUG", delphi_version)
            print(Fore.GREEN + "OK" + Fore.RESET)
        except Exception as e:
            ret = False
            print(Fore.RED + "\n\nBUILD ERROR")
            print(Fore.RESET)
            print(e)
            raise

        # if res.ok:
        #     print(Fore.GREEN + "OK" + Fore.RESET)
        # else:
        #     ret = False
        #     print(Fore.RED + "\n\nBUILD ERROR")
        #     print(Fore.RESET + res.stdout)
        #     print("\n")

    return ret


@task()
def tests(ctx, delphi_version=DEFAULT_DELPHI_VERSION):
    """Builds and execute the unit tests"""
    import os

    apppath = os.path.dirname(os.path.realpath(__file__))
    res = True
    testclient = r"unittests\UnitTests.dproj"

    print("\nBuilding Unit Tests")
    build_delphi_project(
        ctx, (testclient, "Win32"), config="CI", delphi_version=delphi_version
    )

    import subprocess

    print("\nExecuting tests...")
    r = subprocess.run([r"unittests\Win32\CI\UnitTests.exe"])
    if r.returncode != 0:
        return Exit("Compilation failed: \n" + str(r.stdout))
    if r.returncode > 0:
        print(r)
        print("Unit Tests Failed")
        return Exit("Unit tests failed")


@task(post=[tests])
def build(ctx, version="DEBUG", delphi_version=DEFAULT_DELPHI_VERSION):
    """Builds LoggerPro"""
    delphi_projects = get_delphi_projects_to_build(delphi_version)
    ret = build_delphi_project_list(ctx, delphi_projects, version, delphi_version)
    if not ret:
        raise Exit("Build failed")


def get_home() -> str:
    return str(Path(__file__).parent)


def inc_version():
    global g_version
    home = get_home()
    from datetime import datetime

    with open(Path(home).joinpath("VERSION.TXT"), "r") as f:
        v = f.readline().strip()

    pieces = v.split(".")
    if len(pieces) != 3:
        raise Exception("Invalid version format in VERSION.TXT")

    g_version = ".".join(pieces[:-1]) + "." + str(int(pieces[2]) + 1)

    print(f"INC VERSION [{v}] => [{g_version}]")

    with open(Path(home).joinpath("VERSION.TXT"), "w") as f:
        f.write(g_version)
        f.write("\nBUILD DATETIME : " + datetime.now().isoformat() + "\n")


@task(pre=[tests, build])
def release(ctx, delphi_version=DEFAULT_DELPHI_VERSION, skip_build=False, no_git=False):
    """Builds all the projects, executes unit/integration tests and create release"""
    global g_version
    print(Fore.RESET)
    if not no_git:
        inc_version()        
        tag_name = g_version.replace(".", "_").replace(" ", "_")
        print("Creating Git tag " + tag_name)
        if not ctx.run("git add -u "):
            raise Exception("Cannot add files to git")
        if not ctx.run(f"git tag {tag_name}"):
            raise Exception("Cannot create git tag")
        if not ctx.run(f'git commit -m "{tag_name}"'):
            raise Exception("Cannot commit on git")
        if not ctx.run(f"git push origin"):
            raise Exception("Cannot push")
        if not ctx.run(f"git push origin {tag_name}"):
            raise Exception("Cannot push tag")
    inc_version()  # generates dev build version
