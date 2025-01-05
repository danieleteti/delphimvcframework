You will need a SQL Server database with a stored procedure that can accept the log item as parameters

e.g.
@LogType int,
@LogTag nvarchar(25),
@LogMessage nvarchar(4096),
@Timestamp datetime,
@TID int

You will also need to configure the DB Connection string in the LoggerProConfig unit.