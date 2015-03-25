

# Module hackney_socket #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-hackney_socket">hackney_socket()</a> ###



<pre><code>
hackney_socket() = #hackney_socket{}
</code></pre>





### <a name="type-sendfile_option">sendfile_option()</a> ###



<pre><code>
sendfile_option() = {chunk_size, non_neg_integer()} | {use_threads, boolean()} | {send_fun, function()}
</code></pre>





### <a name="type-sock_messages">sock_messages()</a> ###



<pre><code>
sock_messages() = {atom(), atom(), atom()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close a TCP socket.</td></tr><tr><td valign="top"><a href="#connect-4">connect/4</a></td><td>connect to a Port using a specific transport.</td></tr><tr><td valign="top"><a href="#connect-5">connect/5</a></td><td></td></tr><tr><td valign="top"><a href="#controlling_process-2">controlling_process/2</a></td><td>Assign a new controlling process <em>Pid</em> to <em>Socket</em>.</td></tr><tr><td valign="top"><a href="#messages-1">messages/1</a></td><td>Atoms used to identify messages in {active, once | true} mode.</td></tr><tr><td valign="top"><a href="#peername-1">peername/1</a></td><td>Return the address and port for the other end of a connection.</td></tr><tr><td valign="top"><a href="#recv-2">recv/2</a></td><td></td></tr><tr><td valign="top"><a href="#recv-3">recv/3</a></td><td>Receive a packet from a socket in passive mode.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>Send a packet on a socket.</td></tr><tr><td valign="top"><a href="#sendfile-2">sendfile/2</a></td><td>Sends the file Filename to Socket.</td></tr><tr><td valign="top"><a href="#sendfile-5">sendfile/5</a></td><td>Sends Bytes from the file referenced by RawFile beginning at Offset to
Socket.</td></tr><tr><td valign="top"><a href="#setopts-2">setopts/2</a></td><td>Set one or more options for a socket.</td></tr><tr><td valign="top"><a href="#shutdown-2">shutdown/2</a></td><td>Immediately close a socket in one or two directions.</td></tr><tr><td valign="top"><a href="#sockname-1">sockname/1</a></td><td>Get the local address and port of a socket.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###


<pre><code>
close(Hackney_socket::<a href="#type-hackney_socket">hackney_socket()</a>) -&gt; ok
</code></pre>
<br />

Close a TCP socket.
<a name="connect-4"></a>

### connect/4 ###


<pre><code>
connect(Transport::atom(), Host::list(), Port::non_neg_integer(), Opts::list()) -&gt; {ok, <a href="#type-hackney_socket">hackney_socket()</a>} | {error, term()}
</code></pre>
<br />

connect to a Port using a specific transport.
<a name="connect-5"></a>

### connect/5 ###


<pre><code>
connect(Transport::atom(), Host::list(), Port::non_neg_integer(), Opts::list(), Timeout::timeout()) -&gt; {ok, <a href="#type-hackney_socket">hackney_socket()</a>} | {error, term()}
</code></pre>
<br />


<a name="controlling_process-2"></a>

### controlling_process/2 ###


<pre><code>
controlling_process(Hackney_socket::<a href="#type-hackney_socket">hackney_socket()</a>, Pid::pid()) -&gt; ok | {error, closed | not_owner | atom()}
</code></pre>
<br />

Assign a new controlling process _Pid_ to _Socket_.
<a name="messages-1"></a>

### messages/1 ###


<pre><code>
messages(Hackney_socket::<a href="#type-hackney_socket">hackney_socket()</a>) -&gt; <a href="#type-sock_messages">sock_messages()</a>
</code></pre>
<br />

Atoms used to identify messages in {active, once | true} mode.
<a name="peername-1"></a>

### peername/1 ###


<pre><code>
peername(Hackney_socket::<a href="#type-hackney_socket">hackney_socket()</a>) -&gt; {ok, {<a href="inet.md#type-ip_address">inet:ip_address()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}} | {error, atom()}
</code></pre>
<br />

Return the address and port for the other end of a connection.
<a name="recv-2"></a>

### recv/2 ###


<pre><code>
recv(HS::<a href="#type-hackney_socket">hackney_socket()</a>, Length::non_neg_integer()) -&gt; {ok, any()} | {error, closed | atom()}
</code></pre>
<br />


<a name="recv-3"></a>

### recv/3 ###


<pre><code>
recv(Hackney_socket::<a href="#type-hackney_socket">hackney_socket()</a>, Length::non_neg_integer(), Timeout::timeout()) -&gt; {ok, any()} | {error, closed | atom()}
</code></pre>
<br />

Receive a packet from a socket in passive mode.
<a name="send-2"></a>

### send/2 ###


<pre><code>
send(Hackney_socket::<a href="#type-hackney_socket">hackney_socket()</a>, Packet::iolist()) -&gt; ok | {error, atom()}
</code></pre>
<br />

Send a packet on a socket.
<a name="sendfile-2"></a>

### sendfile/2 ###


<pre><code>
sendfile(Filename::string(), HS::<a href="#type-hackney_socket">hackney_socket()</a>) -&gt; {ok, integer()} | {error, <a href="inet.md#type-posix">inet:posix()</a> | closed | badarg | not_owner}
</code></pre>
<br />

Sends the file Filename to Socket. Returns {ok, BytesSent} if
successful, otherwise {error, Reason}.
<a name="sendfile-5"></a>

### sendfile/5 ###


<pre><code>
sendfile(Fd::pid() | <a href="file.md#type-fd">file:fd()</a>, Hackney_socket::<a href="#type-hackney_socket">hackney_socket()</a>, Offset::non_neg_integer(), Bytes::non_neg_integer(), Opts::[<a href="#type-sendfile_option">sendfile_option()</a>]) -&gt; {ok, integer()} | {error, <a href="inet.md#type-posix">inet:posix()</a> | closed | badarg | not_owner}
</code></pre>
<br />


Sends Bytes from the file referenced by RawFile beginning at Offset to
Socket. Returns {ok, BytesSent} if successful, otherwise {error, Reason}.
If Bytes is set to 0 all data after the given Offset is sent.



The file used must be opened using the raw flag, and the process calling
sendfile must be the controlling process of the socket. See
hackney_socket:controlling_process/2



If the OS used does not support sendfile, an Erlang fallback using
file:read and hackney_socket:send is used.



The option list can contain the following options:



* chunk_size: The chunk size used by the erlang fallback to send data. If using the
fallback, this should be set to a value which comfortably fits in the
systems memory. Default is 64 MB.

* use_threads Instruct the emulator to use the async thread pool for the
sendfile system call. This could be usefull if the OS you are running on
does not properly support non-blocking sendfile calls. Do note that using
async threads potentially makes your system volnerable to slow client
attacks. If set to true and no async threads are available, the sendfile
call will return {error,einval}. Introduced in Erlang/OTP 17.0. Default is
false.

* send_fun: Function used to send the data to the socket `SendFun(HS,
Data) -> ok | {error, term()}`


<a name="setopts-2"></a>

### setopts/2 ###


<pre><code>
setopts(Hackney_socket::<a href="#type-hackney_socket">hackney_socket()</a>, Opts::list()) -&gt; ok | {error, atom()}
</code></pre>
<br />

Set one or more options for a socket.

__See also:__ [inet:setopts/2](inet.md#setopts-2).
<a name="shutdown-2"></a>

### shutdown/2 ###


<pre><code>
shutdown(Hackney_socket::<a href="#type-hackney_socket">hackney_socket()</a>, How::read | write | read_write) -&gt; ok
</code></pre>
<br />

Immediately close a socket in one or two directions.
<a name="sockname-1"></a>

### sockname/1 ###


<pre><code>
sockname(Hackney_socket::<a href="#type-hackney_socket">hackney_socket()</a>) -&gt; {ok, {<a href="inet.md#type-ip_address">inet:ip_address()</a>, <a href="inet.md#type-port_number">inet:port_number()</a>}} | {error, atom()}
</code></pre>
<br />

Get the local address and port of a socket
