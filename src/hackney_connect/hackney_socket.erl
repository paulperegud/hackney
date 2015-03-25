-module(hackney_socket).

-export([messages/1,
         connect/4, connect/5,
         recv/3, recv/2,
         send/2,
         setopts/2,
         controlling_process/2,
         peername/1,
         close/1,
         shutdown/2,
         sockname/1,
         sendfile/2, sendfile/5]).

-include("hackney_socket.hrl").

-type hackney_socket() :: #hackney_socket{}.
-type sock_messages() :: {atom(), atom(), atom()}.
-type sendfile_option() :: {chunk_size, non_neg_integer()}
                            | {use_threads, boolean()}
                            | {send_fun, function()}.

-export_type([hackney_socket/0,
              sock_messages/0,
              sendfile_option/0]).


%% @doc Atoms used to identify messages in {active, once | true} mode.
-spec messages(hackney_socket()) -> sock_messages().
messages(#hackney_socket{transport=Transport, sock=Sock}) ->
    Transport:messages(Sock).

%% @doc connect to a Port using a specific transport.
-spec connect(atom(), list(), non_neg_integer(), list()) ->
    {ok, hackney_socket()}
    | {error, term()}.
connect(Transport, Host, Port, Opts) ->
    connect(Transport, Host, Port, Opts, infinity).

-spec connect(atom(), list(), non_neg_integer(), list(), timeout()) ->
    {ok, hackney_socket()}
    | {error, term()}.
connect(Transport, Host, Port, Opts, Timeout) ->
    case Transport:connect(Host, Port, Opts, Timeout) of
        {ok, Sock} ->
            {ok, #hackney_socket{transport=Transport,
                               sock=Sock}};
        Error ->
            Error
    end.

-spec recv(hackney_socket(), non_neg_integer()) ->
    {ok, any()} | {error, closed | atom()}.
recv(HS, Length) ->
    recv(HS, Length, infinity).

%% @doc Receive a packet from a socket in passive mode.
-spec recv(hackney_socket(), non_neg_integer(), timeout()) ->
    {ok, any()} | {error, closed | atom()}.
recv(#hackney_socket{transport=T, sock=S}, Length, Timeout) ->
    T:recv(S, Length, Timeout).

%% @doc Send a packet on a socket.
-spec send(hackney_socket(), iolist()) -> ok | {error, atom()}.
send(#hackney_socket{transport=T, sock=S}, Packet) ->
    T:send(S, Packet).

%% @doc Set one or more options for a socket.
%% @see inet:setopts/2
-spec setopts(hackney_socket(), list()) -> ok | {error, atom()}.
setopts(#hackney_socket{transport=T, sock=S}, Opts) ->
    T:setopts(S, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
-spec controlling_process(hackney_socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(#hackney_socket{transport=T, sock=S}, Pid) ->
	T:controlling_process(S, Pid).

%% @doc Return the address and port for the other end of a connection.
-spec peername(hackney_socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(#hackney_socket{transport=T, sock=S}) ->
	T:peername(S).

%% @doc Close a TCP socket.
-spec close(hackney_socket()) -> ok.
close(#hackney_socket{transport=T, sock=S}) ->
    T:close(S).

%% @doc Immediately close a socket in one or two directions.
-spec shutdown(hackney_socket(), read | write | read_write) -> ok.
shutdown(#hackney_socket{transport=T, sock=S}, How) ->
    T:shutdown(S, How).

%% @doc Get the local address and port of a socket
-spec sockname(hackney_socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(#hackney_socket{transport=T, sock=S}) ->
    T:sockname(S).


%% @doc Sends the file Filename to Socket. Returns {ok, BytesSent} if
%% successful, otherwise {error, Reason}.
-spec sendfile(string(), hackney_socket()) ->
    {ok, integer()} | {error, inet:posix() | closed | badarg | not_owner}.
sendfile(Filename, HS) ->
    case file:open(Filename, [read, raw, binary]) of
        {ok, Fd} ->
            try
                sendfile(Fd, HS, 0, 0, [])
            after
                file:close(Fd)
            end;
        Error ->
            Error
    end.

%% @doc Sends Bytes from the file referenced by RawFile beginning at Offset to
%% Socket. Returns {ok, BytesSent} if successful, otherwise {error, Reason}.
%% If Bytes is set to 0 all data after the given Offset is sent.
%%
%% The file used must be opened using the raw flag, and the process calling
%% sendfile must be the controlling process of the socket. See
%% hackney_socket:controlling_process/2
%%
%% If the OS used does not support sendfile, an Erlang fallback using
%% file:read and hackney_socket:send is used.
%%
%% The option list can contain the following options:
%%
%% <ul>
%% <li>chunk_size: The chunk size used by the erlang fallback to send data. If using the
%% fallback, this should be set to a value which comfortably fits in the
%% systems memory. Default is 64 MB. </li>
%% <li>use_threads Instruct the emulator to use the async thread pool for the
%% sendfile system call. This could be usefull if the OS you are running on
%% does not properly support non-blocking sendfile calls. Do note that using
%% async threads potentially makes your system volnerable to slow client
%% attacks. If set to true and no async threads are available, the sendfile
%% call will return {error,einval}. Introduced in Erlang/OTP 17.0. Default is
%% false.</li>
%% <li>send_fun: Function used to send the data to the socket <code>SendFun(HS,
%% Data) -> ok | {error, term()}</code></li>
%% </ul>
-spec sendfile(pid() | file:fd(), hackney_socket(), non_neg_integer(),
               non_neg_integer(), [sendfile_option()]) ->
    {ok, integer()} | {error, inet:posix() | closed | badarg | not_owner}.
sendfile(Fd, #hackney_socket{transport=T}=HS, Offset, Bytes, Opts)
  when T =:= hackney_tcp ->
    case proplists:get_value(send_fun, Opts) of
        undefined ->
            file:sendfile(Fd, HS#hackney_socket.sock, Offset, Bytes,
                          Opts);
        F when is_function(F) ->
            sendfile_fallback(Fd, HS, Offset, Bytes, Opts)
    end;
sendfile(Fd, HS, Offset, Bytes, Opts) ->
    sendfile_fallback(Fd, HS, Offset, Bytes, Opts).



sendfile_fallback(Fd, HS, Offset, Bytes, Opts) ->
    ChunkSize = proplists:get_value(chunk_size, Opts, ?CHUNK_SIZE),
    {ok, CurrPos} = file:position(Fd, {cur, 0}),
    {ok, _NewPos} = file:position(Fd, {bof, Offset}),
    SendFun = case proplists:get_value(send_fun, Opts) of
                  undefined ->
                      fun(_HS, Data) ->
                              send(HS, Data)
                      end;
                  F when is_function(F) ->
                      F
              end,
    Res = sendfile_fallback1(Fd, HS, SendFun, Bytes, ChunkSize, 0),
    file:position(Fd, {bof, CurrPos}),
    Res.


sendfile_fallback1(Fd, HS, SendFun, Bytes, ChunkSize, Sent)
  when Bytes > Sent; Bytes =:= 0 ->
    %% calculate the size of the data to read
    Length = if
                 Bytes > 0 -> erlang:min(ChunkSize, Bytes - Sent);
                 true -> ChunkSize
             end,
    %% read the data and send it.
    case file:read(Fd, Length) of
        {ok, Data} ->
            Len = iolist_size(Data),
            case SendFun(HS, Data) of
                ok ->
                    sendfile_fallback1(Fd, HS, SendFun, Bytes, ChunkSize,
                                       Sent + Len);
                Error ->
                    Error
            end;
        eof ->
            {ok, Sent};
        Error ->
            Error
    end;
sendfile_fallback1(_, _, _, _, _, Sent) ->
    {ok, Sent}.
