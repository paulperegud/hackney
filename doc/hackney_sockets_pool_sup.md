

# Module hackney_sockets_pool_sup #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`supervisor`](supervisor.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###


<pre><code>
init(X1::[]) -&gt; {ok, {{simple_one_for_one, 4, 3600}, [{hackney_sockets_pool, {hackney_sockets_pool, start_link, []}, temporary, 30000, worker, [hackney_sockets_pool]}]}}
</code></pre>
<br />


<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>
<br />


