<div id="content">

<h2>Appender de email</h2>

<p>Este appender pode ser usado para enviar solicita&ccedil;&otilde;es de log por email.
Uma mensagem de email &eacute; enviada para cada solicita&ccedil;&atilde;o de log.</p>

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
    <li><code>from</code>:<br /> O remetente da mensagem de email.</li>
    
    <li><code>rcpt</code>:<br /> O destinat&aacute;rio da mensagem de email. Uma string ou uma tabela Lua num&eacute;rica com v&aacute;rias strings.</li>
    
    <li><code>user</code>:<br /> O usu&aacute;rio para autentica&ccedil;&atilde;o.</li>
    
    <li><code>password</code>:<br /> A senha para autentica&ccedil;&atilde;o.</li>
    
    <li><code>server</code>:<br /> O servidor ao qual conectar.
    O padr&atilde;o &eacute; <code>&quot;localhost&quot;</code>.</li>
    
    <li><code>port</code>:<br /> A porta &agrave; qual conectar.
    O padr&atilde;o &eacute; <code>25</code>.</li>
    
    <li><code>domain</code>:<br /> O nome do dom&iacute;nio usado para acessar o servidor.
    Usa como padr&atilde;o o nome do host do computador local.</li>
    
    <li><code>headers.to</code>:<br /> O destinat&aacute;rio da mensagem, como uma
    descri&ccedil;&atilde;o extensa.</li>
    
    <li><code>headers.from</code>:<br /> O remetente da mensagem, como uma
    descri&ccedil;&atilde;o extensa.</li>
    
    <li><code>headers.subject</code>:<br /> O assunto da mensagem enviada. Pode conter
    padr&otilde;es como o par&acirc;metro <code>logPattern</code>.</li>
    
    <li><code>logPattern</code>:<br /> &Eacute; poss&iacute;vel especificar um
    padr&atilde;o para controlar o modo como a mensagem &eacute; gravada.<br />
    O valor padr&atilde;o &eacute; <code>&quot;%date %level %message\n&quot;</code>.</li>
</ul>

<h2>Exemplo</h2>

<pre class="example">
require&quot;logging.email&quot;

local logger = logging.email {
  rcpt = &quot;mail@host.com&quot;,
  from = &quot;mail@host.com&quot;,
  headers = { 
    subject = &quot;[%level] logging.email test&quot;, 
  },
}

logger:info(&quot;teste de logging.sql&quot;)
logger:debug(&quot;depurando...&quot;)
logger:error(&quot;erro!&quot;)
</pre>

</div> <!-- id="content" -->
