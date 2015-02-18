<div id="content">

<h2><a name="introduction"></a>Introduction</h2>

<p>LuaLogging provides a simple API to use logging features in Lua.
Its design was based on
<a href="http://logging.apache.org/log4j/docs/index.html">log4j</a>.
LuaLogging currently supports console, file, email, socket and sql
outputs through the use of <em><a href="#appenders">appenders</a></em>.</p>

<p>LuaLogging defines one single global variable, a table called
<code>logging</code> which holds a function to create new
<a href="#logger"><code>logger</code></a> objects.</p>

<p>This logger constructor receives a function
(known as the <em>appender</em> function)
that will be called on each call to log a message.</p>

<p>An <em>appender</em> function receives three arguments:</p>

<ul>
    <li><strong>self</strong>: the logger object</li>
    <li><strong>level</strong>: the logging level</li>
    <li><strong>message</strong>: the message to be logged</li>
</ul>

<h2><a name="installation"></a>Installation</h2>

<p>
LuaLogging follows the
<a href="http://www.inf.puc-rio.br/~roberto/pil2/chapter15.pdf">package model</a>
for Lua 5.1, therefore it should be "installed" in you <code>package.path</code>
</p>

<h2><a name="logger"></a>Logger objects</h2>

<p>A logger object offers the following methods that writes log messages.</p>

<p>For each of the methods below, the parameter <code>message</code> may be any lua value,
not only strings. When necessary <code>message</code> is converted to a string.</p>

<p>The parameter <code>level</code> can be one of the variables enumerated below.
The values are presented in descending criticality, so if the minimum level is
defined as <code>logging.WARN</code> then <code>logging.INFO</code> and
<code>logging.DEBUG</code> levels messages are not logged.</p>

<dl class="reference">
    <dt><strong>logging.DEBUG</strong></dt>
    <dd>The <em>DEBUG</em> level designates fine-grained informational events that
    are most useful to debug an application.</dd>
    
    <dt><strong>logging.INFO</strong></dt>
    <dd>The <em>INFO</em> level designates informational messages that highlight the
    progress of the application at coarse-grained level.</dd>
    
    <dt><strong>logging.WARN</strong></dt>
    <dd>The <em>WARN</em> level designates potentially harmful situations.</dd>
    
    <dt><strong>logging.ERROR</strong></dt>
    <dd>The <em>ERROR</em> level designates error events that might still allow the
    application to continue running.</dd>
    
    <dt><strong>logging.FATAL</strong></dt>
    <dd>The <em>FATAL</em> level designates very severe error events that would
    presumably lead the application to abort.</dd>
</dl>

<h3>Methods</h3>

<dl class="reference">
    <dt><strong>logger:log (level, [message]|[table]|[format, ...]|[function, ...])</strong></dt>
    <dd>Logs a message with the specified level.</dd>
    
    <dt><strong>logger:debug ([message]|[table]|[format, ...]|[function, ...])</strong></dt>
    <dd>Logs a message with DEBUG level.</dd>
    
    <dt><strong>logger:info ([message]|[table]|[format, ...]|[function, ...])</strong></dt>
    <dd>Logs a message with INFO level.</dd>
    
    <dt><strong>logger:warn ([message]|[table]|[format, ...]|[function, ...])</strong></dt>
    <dd>Logs a message with WARN level.</dd>
    
    <dt><strong>logger:error ([message]|[table]|[format, ...]|[function, ...])</strong></dt>
    <dd>Logs a message with ERROR level.</dd>
    
    <dt><strong>logger:fatal ([message]|[table]|[format, ...]|[function, ...])</strong></dt>
    <dd>Logs a message with FATAL level.</dd>
    
    <dt><strong>logger:setLevel (level)</strong></dt>
    <dd>This method sets a minimum level for messages to be logged.</dd>
</dl>

<h2><a name="examples"></a>Examples</h2>

<p>The example below creates a logger that prints the level and message
to the standard output (or whatever the print function does).</p>

<pre class="example">
require "logging"

local logger = logging.new(function(self, level, message)
                             print(level, message)
                             return true
                           end)
                           
logger:setLevel (logging.WARN)
logger:log(logging.INFO, "sending email")

logger:info("trying to contact server")
logger:warn("server did not responded yet")
logger:error("server unreachable")

-- dump a table in a log message
local tab = { a = 1, b = 2 }
logger:debug(tab)

-- use string.format() style formatting
logger:info("val1='%s', val2=%d", "string value", 1234)

-- complex log formatting.
local function log_callback(val1, val2)
	-- Do some complex pre-processing of parameters, maybe dump a table to a string.
	return string.format("val1='%s', val2=%d", val1, val2)
end
-- function 'log_callback' will only be called if the current log level is "DEBUG"
logger:debug(log_callback, "string value", 1234)

</pre>

<p>Upon execution of the above example the following lines will
show in the standard output. Notice that the <em>INFO</em> log requests
are not handled because the minimum level is set to <em>WARN</em>.</p>

<pre class="example">
WARN server did not responded yet
ERROR server unreachable
</pre>

<a name="appenders"></a> 

<h2>Appenders</h2>

The following appenders are included in the standard distribution. 

<ul>
<li><a href="console.html">Console</a></li>
<li><a href="file.html">File</a></li>
<li><a href="rolling_file.html">Rolling File</a></li>
<li><a href="sql.html">SQL</a></li>
<li><a href="socket.html">Socket</a></li>
<li><a href="email.html">Email</a></li>
</ul>

<h2>Upgrading from 1.0.0</h2>

<p>Upgrading from LuaLogging 1.0.0 is very easy. The
<code>logger</code> object is fully compatible. You just need to
change the code that creates the object.</p>

<p>The <code>logger</code> constructor from 1.0.0 received a single
argument which was a filename. To upgrade to 1.1.0 you should
create a <code>logging.file</code> object instead, passing the
filename as argument. As simple as this.</p>

</div> <!-- id="content" -->
