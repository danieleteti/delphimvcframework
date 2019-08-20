import subprocess
import os
from datetime import datetime
from colorama import *
init() #colorama initialization

### task setup env
DOIT_CONFIG = {'verbosity': 2, 'default_tasks': ['build']}

###############################################################################################
############## CONFIGURATION ##################################################################
###############################################################################################
projects = [
			('samples\\01_global_logger\\global_logger.dproj','Win32'),
			('samples\\02_file_appender\\file_appender.dproj','Win32'),
			('samples\\03_console_appender\\console_appender.dproj','Win32'),
			('samples\\04_outputdebugstring_appender\\outputdebugstring_appender.dproj','Win32'),
			('samples\\05_vcl_appenders\\vcl_appenders.dproj','Win32'),
			('samples\\08_email_appender\\email_appender.dproj','Win32'),
			('samples\\10_multiple_appenders\\multiple_appenders.dproj','Win32'),
			('samples\\15_appenders_with_different_log_levels\\multi_appenders_different_loglevels.dproj','Win32'),
			('samples\\20_multiple_loggers\\multiple_loggers.dproj','Win32'),
			('samples\\50_custom_appender\\custom_appender.dproj','Win32'),
			('samples\\60_logging_inside_dll\\MainProgram.dproj','Win32'),
			('samples\\60_logging_inside_dll\\mydll.dproj','Win32'),
		    ('samples\\70_isapi_sample\\loggerproisapisample.dproj','Win32'),
			('samples\\90_remote_logging_with_redis\\REDISAppenderSample.dproj','Win32'),
			('samples\\90_remote_logging_with_redis\\redis_logs_viewer\\REDISLogsViewer.dproj','Win32'),
            ('samples\\100_udp_syslog\\udp_syslog.dproj','Win32'),
            ('samples\\110_rest_appender\RESTAppenderSample.dproj','Win32'),
            ('samples\\110_rest_appender_mobile\RESTAppenderMobileSample.dproj','Android'),
            ('samples\\120_elastic_search_appender\\ElasticSearchAppenderSample.dproj','Win32'),
            ('samples\\rest_logs_collector\RESTLogsCollector.dproj','Win32')
]

release_path = "BUILD"
###############################################################################################
############## END CONFIGURATION ##############################################################
###############################################################################################

GlobalBuildVersion = 'DEV' #if we are building an actual release, this will be replaced

def header(headers):    
    elements = None
    if type(headers).__name__ == 'str':
        elements = [headers]
    else:
        elements = headers

    print(Style.BRIGHT + Back.WHITE + Fore.RED + "*" * 80 + Style.RESET_ALL)
    for txt in elements:
        s = '{:^80}'.format(txt)
        print(Style.BRIGHT + Back.WHITE + Fore.RED + s + Style.RESET_ALL)       
    print(Style.BRIGHT + Back.WHITE + Fore.RED + "*" * 80 + Style.RESET_ALL)        
    

def buildProject(project, config = 'DEBUG'):
    project_file, platform = project
    header(["Building", project_file,"(config " + config + ")"])
    p = project_file.replace('.dproj', '.cfg')
    if os.path.isfile(p):
      if os.path.isfile(p + '.unused'):
        os.remove(p + '.unused')
      os.rename(p, p + '.unused')
    return subprocess.call(f"rsvars.bat & msbuild /t:Build /p:Config={config} /p:Platform={platform} \"{project_file}\"", shell=True) == 0

def buildProjects():
    res = True
    for project in projects:
      res &= buildProject(project)
      if not res:
        print("Failed " + project[0])
    return res


def build_unit_tests():
    res = buildProject(('unittests\\UnitTests.dproj','Win32'), 'PLAINDUNITX')
    return res


def create_build_tag(version):
    global GlobalBuildVersion
    GlobalBuildVersion = version
    header("BUILD VERSION: " + GlobalBuildVersion)
    f = open("VERSION.TXT","w")
    f.write("VERSION " + GlobalBuildVersion + "\n")
    f.write("BUILD DATETIME " + datetime.now().isoformat() + "\n")
    f.close()

#############################################################################################################################

def task_build():
    '''Use: doit build -v <VERSION> -> Builds all the projects. Then creates SFX archive.'''    
    return {
        'actions': [
						create_build_tag,
						"echo %%date%% %%time:~0,8%% > LOGGERPRO-BUILD-TIMESTAMP.TXT",            
						buildProjects,
						build_unit_tests,
						"unittests\\Win32\\PLAINDUNITX\\UnitTests.exe -exit:Continue"],
	'params':[{'name':'version',
	           'short':'v',
	           'long':'version',
             'type':str,
             'default':'DEVELOPMENT'}
             ],						
        'verbosity': 2
        }

def task_unittests():
    '''Use: doit unittests. Builds unittests project and run it.'''    
    return {
        'actions': [
					build_unit_tests,
					"unittests\\Win32\\PLAINDUNITX\\UnitTests.exe -exit:Continue"
					],
				'params':[{'name':'version',
					'short':'v',
					'long':'version',
					'type':str,
					'default':'DEVELOPMENT'}
        ],
        'verbosity': 2
        }
				