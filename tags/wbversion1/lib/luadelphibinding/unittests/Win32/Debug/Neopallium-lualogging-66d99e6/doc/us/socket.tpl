<div id="content">

<h2>Socket appender</h2>

This appender can be used to send log requests through a socket.
Socket appender relies on <a href=
"http://www.tecgraf.puc-rio.br/luasocket">LuaSocket</a> to do its
job.<br />
Upon each log request a connection is opened, the message is sent
and the connection is closed. 

<pre class="example">
function logging.socket(address, port [,logPattern])
</pre>

<ul>
    <li><code>address</code>:<br />
    Address can be an IP address or a host name for which the log
    message will be sent.</li>
    
    <li><code>port</code>:<br />
    The port must be an integer number in the range [1..64K).</li>
    
    <li><code>logPattern</code>:<br />
    A pattern can be specified to control how the message is
    written.<br />
    The default value is <code>"%date %level %message\n"</code>.</li>
</ul>

<h2>Example</h2>

<pre class="example">
require"logging.socket"

local logger = logging.socket("localhost", 5000)

logger:info("logging.socket test")
logger:debug("debugging...")
logger:error("error!")
</pre>

<p>&nbsp;</p>
<p>&nbsp;</p>
<p>&nbsp;</p>
<p>&nbsp;</p>

</div> <!-- id="content" -->
