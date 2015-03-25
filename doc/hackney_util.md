

# Module hackney_util #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#filter_options-3">filter_options/3</a></td><td>filter a proplists and only keep allowed keys.</td></tr><tr><td valign="top"><a href="#increment-1">increment/1</a></td><td></td></tr><tr><td valign="top"><a href="#increment-2">increment/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_ipv6-1">is_ipv6/1</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_apply_defaults-2">maybe_apply_defaults/2</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_seed-0">maybe_seed/0</a></td><td></td></tr><tr><td valign="top"><a href="#mod_metrics-0">mod_metrics/0</a></td><td></td></tr><tr><td valign="top"><a href="#privdir-0">privdir/0</a></td><td></td></tr><tr><td valign="top"><a href="#rand_increment-1">rand_increment/1</a></td><td></td></tr><tr><td valign="top"><a href="#rand_increment-2">rand_increment/2</a></td><td></td></tr><tr><td valign="top"><a href="#require-1">require/1</a></td><td>Start the given applications if they were not already started.</td></tr><tr><td valign="top"><a href="#set_option_default-3">set_option_default/3</a></td><td>set the default options in a proplists if not defined.</td></tr><tr><td valign="top"><a href="#to_atom-1">to_atom/1</a></td><td></td></tr><tr><td valign="top"><a href="#user_opts-2">user_opts/2</a></td><td></td></tr><tr><td valign="top"><a href="#user_opts-3">user_opts/3</a></td><td></td></tr><tr><td valign="top"><a href="#valid_opts-2">valid_opts/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="filter_options-3"></a>

### filter_options/3 ###


<pre><code>
filter_options(Tail::[{atom(), any()} | {raw, any(), any(), any()}], AllowedKeys::[atom()], Acc) -&gt; Acc
</code></pre>

<ul class="definitions"><li><code>Acc = [any()]</code></li></ul>

filter a proplists and only keep allowed keys
<a name="increment-1"></a>

### increment/1 ###


<pre><code>
increment(N::pos_integer()) -&gt; pos_integer()
</code></pre>
<br />


<a name="increment-2"></a>

### increment/2 ###


<pre><code>
increment(N, Max) -&gt; pos_integer()
</code></pre>

<ul class="definitions"><li><code>N = pos_integer()</code></li><li><code>Max = pos_integer()</code></li></ul>


<a name="is_ipv6-1"></a>

### is_ipv6/1 ###

`is_ipv6(Host) -> any()`


<a name="maybe_apply_defaults-2"></a>

### maybe_apply_defaults/2 ###

`maybe_apply_defaults(Rest, Options) -> any()`


<a name="maybe_seed-0"></a>

### maybe_seed/0 ###

`maybe_seed() -> any()`


<a name="mod_metrics-0"></a>

### mod_metrics/0 ###

`mod_metrics() -> any()`


<a name="privdir-0"></a>

### privdir/0 ###

`privdir() -> any()`


<a name="rand_increment-1"></a>

### rand_increment/1 ###


<pre><code>
rand_increment(N::pos_integer()) -&gt; pos_integer()
</code></pre>
<br />


<a name="rand_increment-2"></a>

### rand_increment/2 ###


<pre><code>
rand_increment(N, Max) -&gt; pos_integer()
</code></pre>

<ul class="definitions"><li><code>N = pos_integer()</code></li><li><code>Max = pos_integer()</code></li></ul>


<a name="require-1"></a>

### require/1 ###


<pre><code>
require(Rest::[module()]) -&gt; ok
</code></pre>
<br />

Start the given applications if they were not already started.
<a name="set_option_default-3"></a>

### set_option_default/3 ###


<pre><code>
set_option_default(Opts, Key::atom(), Value::any()) -&gt; Opts
</code></pre>

<ul class="definitions"><li><code>Opts = [{atom(), any()}]</code></li></ul>

set the default options in a proplists if not defined
<a name="to_atom-1"></a>

### to_atom/1 ###

`to_atom(V) -> any()`


<a name="user_opts-2"></a>

### user_opts/2 ###

`user_opts(Type, Opts) -> any()`


<a name="user_opts-3"></a>

### user_opts/3 ###

`user_opts(Type, Opts, Default) -> any()`


<a name="valid_opts-2"></a>

### valid_opts/2 ###

`valid_opts(Type, Default) -> any()`


