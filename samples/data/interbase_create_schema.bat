set ISQL=C:\Program Files (x86)\Embarcadero\InterBase\bin\isql.exe
del ACTIVERECORDDB.IB
"%ISQL%" -user sysdba -password masterkey -input C:\DEV\dmvcframework\samples\data\activerecorddb_interbase_script.sql C:\ProgramData\Embarcadero\InterBase\gds_db\examples\database\EMPLOYEE.GDB