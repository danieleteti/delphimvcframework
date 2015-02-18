<div id="content">

<h2>SQL appender</h2>

<p>The SQL appender can be used to write log messages to a SQL
database table. It uses <a href="http://www.keplerproject.org/luasql/">LuaSQL</a>, 
therefore any database supported by LuaSQL can be used.</p>

<pre class="example">
function logging.sql{
    connectionfactory = <i>function</i>,
    [tablename = <i>string</i>,]
    [logdatefield = <i>string</i>,]
    [loglevelfield = <i>string</i>,]
    [logmessagefield = <i>string</i>,]
    [keepalive = <i>boolean</i>],
}
</pre>

<ul>
    <li><code>connectionfactory</code>:<br />
    This must be a function that creates a LuaSQL connection object.
    This function will be called everytime a connection needs to be
    created.</li>
    
    <li><code>tablename</code>:<br />
    The name of the table to write the log requests. Default value is
    <code>"LogTable"</code>.</li>
    
    <li><code>logdatefield</code>:<br />
    The name of the field to write the date of each log request.
    Default value is <code>"LogDate"</code>.</li>
    
    <li><code>loglevelfield</code>:<br />
    The name of the field to write the level of each log request.
    Default value is <code>"LogLevel"</code>.</li>
    
    <li><code>logmessagefield</code>:<br />
    The name of the field to write the message of each log request.
    Default value is <code>"LogMessage"</code>.</li>
    
    <li><code>keepalive</code>:<br />
    In every log request a connection to the database is opened, the
    message is written, and the connection is closed.<br />
    If the user wants to keep the connection opened he can specify
    <code>keepalive = true</code>.</li>
</ul>

<h2>Example</h2>

<pre class="example">
require"logging.sql"
require"luasql.jdbc"

local env, err = luasql.jdbc('com.mysql.jdbc.Driver')

local logger = logging.sql {
  connectionfactory = function()
    local con, err = env:connect('jdbc:mysql://localhost/test',
                                 'tcp', '123')
    assert(con, err)
    return con
  end,
  keepalive = true,
}

logger:info("logging.sql test")
logger:debug("debugging...")
logger:error("error!")
</pre>

</div> <!-- id="content" -->
