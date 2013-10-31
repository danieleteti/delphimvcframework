<div id="content">

<h2>Console appender</h2>

<p>Console is the simplest appender. It just writes the log messages
to <code>io.stdout</code>.</p>

<pre class="example">
function logging.console([logPattern])
</pre>

<ul>
    <li><code>logPattern</code>:<br />
    A pattern can be specified to control how the message is
    written.<br />
    The default value is <code>"%date %level %message\n"</code>.</li>
</ul>

<h2>Examples</h2>

<pre class="example">
require"logging.console"

local logger = logging.console()

logger:info("logging.console test")
logger:debug("debugging...")
logger:error("error!")
</pre>

<p>&nbsp;</p>
<p>&nbsp;</p>
<p>&nbsp;</p>
<p>&nbsp;</p>
<p>&nbsp;</p>
<p>&nbsp;</p>
<p>&nbsp;</p>

</div> <!-- id="content" -->
