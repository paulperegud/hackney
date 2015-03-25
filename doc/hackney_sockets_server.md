

# Module hackney_sockets_server #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#decr_active_pool-1">decr_active_pool/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_pool_size-1">get_pool_size/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_socket_pool-1">get_socket_pool/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#incr_active_pool-1">incr_active_pool/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#init_pool-2">init_pool/2</a></td><td></td></tr><tr><td valign="top"><a href="#int_to_max-1">int_to_max/1</a></td><td></td></tr><tr><td valign="top"><a href="#max_to_int-1">max_to_int/1</a></td><td></td></tr><tr><td valign="top"><a href="#pool_info-1">pool_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#reset_pool_count-1">reset_pool_count/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_pool_size-2">set_pool_size/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_socket_pool-2">set_socket_pool/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#with_pool-2">with_pool/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`


<a name="decr_active_pool-1"></a>

### decr_active_pool/1 ###

`decr_active_pool(PoolName) -> any()`


<a name="get_pool_size-1"></a>

### get_pool_size/1 ###

`get_pool_size(PoolName) -> any()`


<a name="get_socket_pool-1"></a>

### get_socket_pool/1 ###

`get_socket_pool(PoolName) -> any()`


<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Msg, From, State) -> any()`


<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`


<a name="incr_active_pool-1"></a>

### incr_active_pool/1 ###

`incr_active_pool(PoolName) -> any()`


<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`


<a name="init_pool-2"></a>

### init_pool/2 ###

`init_pool(PoolName, PoolSize) -> any()`


<a name="int_to_max-1"></a>

### int_to_max/1 ###

`int_to_max(Max) -> any()`


<a name="max_to_int-1"></a>

### max_to_int/1 ###

`max_to_int(Max) -> any()`


<a name="pool_info-1"></a>

### pool_info/1 ###

`pool_info(PoolName) -> any()`


<a name="reset_pool_count-1"></a>

### reset_pool_count/1 ###

`reset_pool_count(PoolName) -> any()`


<a name="set_pool_size-2"></a>

### set_pool_size/2 ###

`set_pool_size(PoolName, PoolSize) -> any()`


<a name="set_socket_pool-2"></a>

### set_socket_pool/2 ###

`set_socket_pool(PoolName, Pid) -> any()`


<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`


<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


<a name="with_pool-2"></a>

### with_pool/2 ###

`with_pool(PoolName, Fun) -> any()`


