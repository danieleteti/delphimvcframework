<div id="content">

<h2>Email appender</h2>

<p>This appender can be used to send log requests through email. One email
message is sent for each log request.</p>

<pre class="example">
function logging.email {
    from = <i>string</i>,
    rcpt = <i>string</i> or <i>string-table</i>,
    [user = <i>string</i>,]
    [password = <i>string</i>,]
    [server = <i>string</i>,]
    [port = <i>number</i>,]
    [domain = <i>string</i>,]
    [headers = <i>table</i>,]
    [logPattern = <i>string</i>,]
}
</pre>

<ul>
    <li><code>from</code>:<br />
    The sender of the email message.</li>
    
    <li><code>rcpt</code>:<br />
    The recipient of the email message. A string or a numerically indexed Lua table with strings.</li>
    
    <li><code>user</code>:<br />
    User for authentication.</li>
    
    <li><code>password</code>:<br />
    Password for authentication.</li>
    
    <li><code>server</code>:<br />
    Server to connect to. Default is <code>"localhost"</code>.</li>
    
    <li><code>port</code>:<br />
    Port to connect to. Default is <code>25</code>.</li>
    
    <li><code>domain</code>:<br />
    Domain name used to greet the server. Defaults to the local
    machine host name.</li>
    
    <li><code>headers.to</code>:<br />
    The recipient of the message, as an extended description.</li>
    
    <li><code>headers.from</code>:<br />
    The sender of the message, as an extended description.</li>
    
    <li><code>headers.subject</code>:<br />
    The subject of the message sent. This can contain patterns like
    the <code>logPattern</code> parameter.</li>
    
    <li><code>logPattern</code>:<br />
    A pattern can be specified to control how the message is
    written.<br />
    The default value is <code>"%date %level %message\n"</code>.</li>
</ul>

<h2>Example</h2>

<pre class="example">
require"logging.email"

local logger = logging.email {
  rcpt = "mail@host.com",
  from = "mail@host.com",
  headers = { 
    subject = "[%level] logging.email test", 
  },
}

logger:info("logging.sql test")
logger:debug("debugging...")
logger:error("error!")
</pre>

</div> <!-- id="content" -->
