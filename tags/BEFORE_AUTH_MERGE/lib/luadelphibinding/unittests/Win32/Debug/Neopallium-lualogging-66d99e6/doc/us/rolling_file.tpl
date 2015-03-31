<div id="content">

<h2>Rolling File appender</h2>

<p>The rolling file appender can be used to write log messages to a file. It
uses Lua I/O routines to do its job. The rolling file appender rolls over the logfile 
once it has reached a certain size limit. It also mantains a maximum number of log files.</p>

<pre class="example">
function logging.rolling_file(filename, maxFileSize, [maxBackupIndex], [logPattern])
</pre>

<ul>
    <li><code>filename</code>:<br />
    The name of the file to be written to.<br />
     If the file cannot be opened for appending the logging request
    returns nil and an error message.</li>
    
    <li><code>maxFileSize</code>:<br />
    The max size of the file in bytes. Every time the file reaches this size
    it will rollover, generating a new clean log file and storing the old log
    on a filename.n, where n goes from 1 to the configured maxBackupIndex.<br />
    The more recent backup is the one with the lowest n on its filename.<br />
    Eg. test.log.1 (most recent backup) <br />
        test.log.2 (least recent backup)
    </li>
    
    <li><code>maxBackupIndex</code>:<br />
    The number of backup files that will be generated.
    The default value is <code>1</code>.</li>

    <li><code>logPattern</code>:<br />
    A pattern can be specified to control how the message is
    written.<br />
    The default value is <code>"%date %level %message\n"</code>.</li>
</ul>

<h2>Example</h2>

<pre class="example">
require"logging.rolling_file"

local logger = logging.rolling_file("test.log", 1024, 5, "%Y-%m-%d")

logger:info("logging.file test")
logger:debug("debugging...")
logger:error("error!")
</pre>

<p>&nbsp;</p>
<p>&nbsp;</p>

</div> <!-- id="content" -->
