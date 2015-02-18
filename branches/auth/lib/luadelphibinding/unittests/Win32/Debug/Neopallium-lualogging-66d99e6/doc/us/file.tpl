<div id="content">

<h2>File appender</h2>

<p>The file appender can be used to write log messages to a file. It
uses Lua I/O routines to do its job.</p>

<pre class="example">
function logging.file(filename, [datePattern], [logPattern])
</pre>

<ul>
    <li><code>filename</code>:<br />
    The name of the file to be written to. On each call to log a
    message the file is opened for appending and closed immediately.<br />
     If the file cannot be opened for appending the logging request
    returns nil and an error message.</li>
    
    <li><code>datePattern</code>:<br />
    This is an optional parameter that can be used to specify a date
    pattern that will be passed to the
    <a href="http://www.lua.org/manual/5.0/manual.html#libiosys"><code>os.date</code></a>
    function to compose the filename.<br />
    This is useful to create daily or monthly log files. If the user
    wants to create one log file per day he specifies a
    <code>"%Y-%m-%d"</code> pattern and a filename like
    <code>"temp%s.log"</code>.</li>
    
    <li><code>logPattern</code>:<br />
    A pattern can be specified to control how the message is
    written.<br />
    The default value is <code>"%date %level %message\n"</code>.</li>
</ul>

<h2>Example</h2>

<pre class="example">
require"logging.file"

local logger = logging.file("test%s.log", "%Y-%m-%d")

logger:info("logging.file test")
logger:debug("debugging...")
logger:error("error!")
</pre>

<p>&nbsp;</p>
<p>&nbsp;</p>

</div> <!-- id="content" -->
