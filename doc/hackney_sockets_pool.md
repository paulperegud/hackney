

# Module hackney_sockets_pool #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#checkin-2">checkin/2</a></td><td></td></tr><tr><td valign="top"><a href="#checkout-5">checkout/5</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_pool-1">start_pool/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_pool-2">start_pool/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="checkin-2"></a>

### checkin/2 ###

`checkin(PoolName, HS) -> any()`


<a name="checkout-5"></a>

### checkout/5 ###

`checkout(PoolName, Transport, Host, Port, Opts) -> any()`


<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`


<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Msg, From, State) -> any()`


<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`


<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`


<a name="start_link-2"></a>

### start_link/2 ###

`start_link(Name, Opts) -> any()`


<a name="start_pool-1"></a>

### start_pool/1 ###

`start_pool(PoolName) -> any()`


<a name="start_pool-2"></a>

### start_pool/2 ###

`start_pool(PoolName, Opts) -> any()`


<a name="stop-1"></a>

### stop/1 ###

`stop(PoolName) -> any()`


<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


