%%% -*- erlang -*-
%%%
%%% This file is part of hackney released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%

%% @doc module handling the request
-module(hackney_http_client).

-export([perform/2,
         send/2, send_multipart/2]).

-include("hackney.hrl").
-include("hackney_lib.hrl").

perform({Method0, Url, Headers0, Body}, Options) ->
    Method = request_method(Method0),
    Path = request_path(Method, Url),
    Headers = request_headers(Url, Headers0, Options),
    ReqType = request_type(Headers),
    ok.




send(Data, Req) ->
   send1(Data, Req).

%% @doc function used to send parts one by one to the remote
send_multipart(eof, #hrequest{boundary=Boundary}=Req) ->
    case send1(hackney_multipart:mp_eof(Boundary), Req) of
        {ok, Req2} -> send1(eof, Req2);
        Error -> Error
    end;
send_multipart({file, Path}=Part, #hrequest{boundary=Boundary}=Req) ->
    {MpHeader, _} = hackney_multipart:mp_file_header(Part, Boundary),
    Stream = [MpHeader, {file, Path}, <<"\r\n">>],
    send1({fun mp_stream/2, Stream}, Req);
send_multipart({file, Path, _ExtraHeaders}=Part,
               #hrequest{boundary=Boundary}=Req) ->
    {MpHeader, _} = hackney_multipart:mp_file_header(Part, Boundary),
    Stream = [MpHeader, {file, Path}, <<"\r\n">>],
    send1({fun mp_stream/2, Stream}, Req);
send_multipart({mp_mixed, Name, MixedBoundary}, Req) ->
    {MpHeader, _} = hackney_multipart:mp_mixed_header(Name, MixedBoundary),
    send1(<< MpHeader/binary, "\r\n" >>, Req);
send_multipart({mp_mixed_eof, MixedBoundary}, Req) ->
    Eof = hackney_multipart:mp_eof(MixedBoundary),
    send1(<< Eof/binary, "\r\n" >>, Req);
send_multipart({part, Name}, Req) ->
    send_multipart({part, Name, []}, Req);
send_multipart({part, Name, ExtraHeaders}, Req) when is_list(ExtraHeaders) ->
    {MpHeader, _} = hackney_multipart:mp_data_header({Name, ExtraHeaders},
                                                     Req#hrequest.boundary),
    send1(MpHeader, Req);
send_multipart({part, Name, Len}, Req) when is_integer(Len) ->
    send_multipart({part, Name, Len, []}, Req);
send_multipart({part, Name, Len, ExtraHeaders}, Req) ->
    {MpHeader, _} = hackney_multipart:mp_data_header({Name, Len, ExtraHeaders},
                                                     Req#hrequest.boundary),
    send1(MpHeader, Req);
send_multipart({part_bin, Bin}, Req) ->
    send1(Bin, Req);
send_multipart(part_eof, Req) ->
    send1(<<"\r\n">>, Req);
send_multipart({Name, Bin}, Req) ->
    send_multipart({Name, Bin, []}, Req);
send_multipart({Name, Bin, ExtraHeaders}, Req) ->
    Len = byte_size(Bin),
    {MpHeader, _} = hackney_multipart:mp_data_header({Name, Len, ExtraHeaders},
                                                     Req#hrequest.boundary),
    send1(<< MpHeader/binary, Bin/binary, "\r\n" >>, Req);
send_multipart({Name, Bin, Disp, ExtraHeaders}, Req) ->
    Len = byte_size(Bin),
    {MpHeader, _} = hackney_multipart:mp_data_header({Name, Len, Disp, ExtraHeaders},
                                                     Req#hrequest.oundary),
    send1(<< MpHeader/binary, Bin/binary, "\r\n" >>, Req);
send_multipart(_, Req) ->
    {error, unknown_part}.


parse(#hrequest{parser=P}=Req) ->
    ok.


process_response({more NParser}, Req) ->
    {more, <<>>, Req#parser{parser=NParser}};
process_response({more, NParser, Buffer}, Req) ->
    {more, Buffer, Req#parser{parser=NParser}};
process_response({response, Version, Status, Reason, Parser}, Req) ->
    NewReq = Req#hrequest{parser=Parser,
                          rversion=Version,
                          rstatus=Status,
                          rreason=Reason},
    {ok, Status, Reason, NewReq};
process_response({header, {Key, Value}, NParser}, Req) ->
    NHeaders = hackney_headers:insert(Key, Value, Req#hrequest.rheaders),
    {continue, Req#hrequest{parser=NParser, rheaders=NHeaders}};
process_response({headers_complete, NParser},
                 #hrequest{method=M, rstatus=S, readers=H}=Req) ->

    NewReq = Req#hrequest{parser=NParser},
    if
        M =:= <<"HEAD">> -> {done, {headers, H}, NewReq};
        S =:= 204; S =:= 304 -> {done, {headers, H}, NewReq};
        true -> {ok, {headers, H}, NewReq}
    end;
process_response({ok, Data, NParser}, Req) ->
    {ok, Data, Req#hrequest{hrequest=Req}};
process_response({done, Rest}, Req) ->
    NewReq = finish_response(Rest, Req),
    {done, NewReq};
process_response(done, Req) ->
    process_response({done, <<>>}, Req);
process_response({error, _}=Error, Req) ->
    {Error, Req};
process_response(Error, Req) ->
    {{error, Error}, Req}.









%% utilif unction to send a stream and continue the request adter it
mp_stream([Data | Stream]) -> {ok, Data, Stream};
mp_stream([]) -> ok.


%% send data to the remote node
send1(eof, #hrequest{sock=HS, reqtype=chunked}=Req) ->
    ok = send_chunk(HS, <<>>),
    {eof, Req};
send1(eof, Req) ->
    {eof, Req}f;
send1({file, Filename}, Req) ->
    sendfile(Filename, [], Req);
send1({file, Filename, Opts}, Req) ->
    sendfile(Filename, Opts, Req);
send1(Fun, #hreques{sock=HS, send_fun=Send}=Req) ->
    case Fun() of
        {ok, Data} ->
            case Send(HS, Data) of
                ok -> send1(Fun, Req);
                Error -> {Error, Req}
            end;
        ok -> {ok, Req};
        eof -> send1(eof, Req)
        Error -> {Error, Req}
    end;
send1({Fun, State}, #hreques{sock=HS, send_fun=Send}=Req) ->
    case Fun(State) of
        {ok, Data, NState} ->
            case Send(HS, Data) of
                ok -> send1({Fun, NState}, Req);
                Error -> {Error, Req}
            end;
        ok -> {ok, Req};
        eof -> send1(eof, Req);
        Error -> {Error, Req}
    end;
send1(Data, #hreques{sock=HS, send_fun=Send}=Req) ->
    {Send(HS, Data), Req}.

%% set body headers and eventually create a stream for parts given to it or
%% set the form as a binary
handle_reqbody(stream, _Method, Headers) ->
    {nil, stream, Headers};
handle_reqbody(stream_multipart, _Method, Headers) ->
    {nil, stream, default_multipart_header(Headers)};
handle_reqbody({stream_multipart, Sz}, _Method, Headers) ->
    {Sz, stream, default_multipart_header(Headers)};
handle_reqbody({stream_multipart, Sz, Boundary}, _Method, Headers) ->
    {Sz, stream, default_multipart_header(Headers, Boundary)};
handle_reqbody(Body, Method, Headers)
  when Body =:= []; Body =:= <<>> ->
    case lists:member(Method, [<<"POST">>, <<"PUT">>, <<"PATCH">>]) of
        true ->
            handle_reqbody1(Body, Headers);
        false ->
            {nil, Body, Headers}
    end;
handle_reqbody(Body, _Method, Headers) ->
    handle_reqbody1(Body, Headers).


%% private function to parse the body
handle_reqbody1({form, KVS}, Headers) ->
    Body = hackney_url:qs(KVS),
    NHeaders = hackney_headers:store(<<"Content-Type">>,
                                     <<"application/x-www-form-urlencoded; charset=utf-8">>,
                                     Headers),
    {erlang:byte_size(Body), Body, NHeaders};
handle_reqbody1({multipart, Parts}, Headers) ->
    Boundary = hackney_multipart:boundary(),
    MpLen = hackney_multipart:len_mp_stream(Parts, Boundary),
    MpStream = make_multipart_stream(Parts, Boundary),
    NHeaders = maybe_store_hdr(<<"Content-Type">>,
                               <<"multipart/form-data; boundary=",
                                 Boundary/binary >>, Headers),
    {MpLen, MpStream, NHeaders};
handle_reqbody1({file, Filename}=Body, Headers) ->
    Sz = filelib:file_size(Filename),
    Mt = hackney_mimetypes:filename(Filename),
    NHeaders = maybe_store_hdr(<<"Content-Type">>, Mt, Headers),
    {Sz, Body, NHeaders};
handle_reqbody1(Fun, Headers) when is_function(Fun) ->
        NHeaders = maybe_store_hdr(<<"content-type">>,
                                   <<"application/octet-stream">>, Headers),
        {undefined, Fun, NHeaders};
handle_reqbody1({Fun, _}=Body, Headers) when is_function(Fun) ->
    NHeaders = maybe_store_hdr(<<"content-type">>,
                               <<"application/octet-stream">>,
                               Headers),
    {undefined, Body, NHeaders};

handle_reqbody1(Body0, Headers) when is_list(Body0); is_binary(Body0) ->
    {Sz, Body} = if
                     is_binary(Body0) -> {size(Body0), Body0};
                     true ->
                         Body1 = iolist_to_binary(Body0),
                         {size(Body1), Body1}
                 end,
    NHeaders = maybe_store_hdr(<<"content-type">>,
                               <<"application/octet-stream">>,
                               Headers),
    {Sz, Body, NHeaders}.

default_multipart_header(Headers) ->
    default_multipart_header(Headers, hackney_multipart:boundary()).

default_multipart_header(Headers, Boundary) ->
    maybe_store_hdr(<<"content-type">>,
                    <<"multipart/form-data; boundary=", Boundary >>,
                    Headers).


make_multipart_stream(Parts, Boundary) ->
    {_, Stream} = lists:foldl(fun mk_part/2, {Boundary, []}, Parts),
    MpEof = hackney_multipart:mp_eof(Boundary),
    {fun mp_stream_fun/1, lists:reverse([MpEof | Stream])}.


mp_stream_fun([Part | Stream]) -> {ok, Part, Stream};
mp_stream_fun([]) -> eof.

mk_part({file, Path}=Part, {Boundary, Stream}) ->
    {MpHeader, _} = hackney_multipart:mp_file_header(Part, Boundary),
    {Boundary, [<<"\r\n">>, {file, Path}, MpHeader | Stream]};
mk_part({file, Path, _ExtraHeaders}=Part, {Boundary, Stream}) ->
    {MpHeader, _} = hackney_multipart:mp_file_header(Part, Boundary),
    {Boundary, [<<"\r\n">>, {file, Path}, MpHeader | Stream]};
mk_part({file, Path, _Disp, _ExtraHeaders}=Part, {Boundary, Stream}) ->
    {MpHeader, _} = hackney_multipart:mp_file_header(Part, Boundary),
    {Boundary, [<<"\r\n">>, {file, Path}, MpHeader | Stream]};
mk_part({mp_mixed, Name, MixedBoundary}, {Boundary, Stream}) ->
    {MpHeader, _} = hackney_multipart:mp_mixed_header(Name, MixedBoundary),
    {Boundary, [<< MpHeader/binary, "\r\n" >> | Stream]};
mk_part({mp_mixed_eof, MixedBoundary}, {Boundary, Stream}) ->
    Eof = hackney_multipart:mp_eof(MixedBoundary),
    {Boundary, [<< Eof/binary, "\r\n" >> | Stream]};
mk_part({Name, Bin, ExtraHeaders}, {Boundary, Stream}) ->
    Len = byte_size(Bin),
    {MpHeader, _} = hackney_multipart:mp_data_header({Name, Len, ExtraHeaders},
                                                     Boundary),
    PartBin = << MpHeader/binary, Bin/binary, "\r\n" >>,
    {Boundary, [PartBin | Stream]};
mk_part({Name, Bin, Disp, ExtraHeaders}, {Boundary, Stream}) ->
    Len = byte_size(Bin),
    {MpHeader, _} = hackney_multipart:mp_data_header({Name, Len, Disp, ExtraHeaders},
                                                     Boundary),
    PartBin = << MpHeader/binary, Bin/binary, "\r\n" >>,
    {Boundary, [PartBin | Stream]}.


maybe_store_hdr(Key, Value, Headers) ->
    case hackney_headers:get_value(Key, Headers) of
        undefined -> hackney_headers:store(Key, Value, Headers);
        _ -> Headers
    end.


request_type(Headers) ->
    TE = hackney_headers:get_value(<<"Transfer-Encoding">>, Headers, <<"">>),
    CLen = hackney_headers:get_value(<<"Content-Length">>, Headers,
                                     undefined),

    case hackney_bstr:to_lower(TE) of
        <<"chunked">> -> chunked;
        _ when CLen =:= undefined -> chunked;
        _ -> identity
    end.



request_headers(Url, Headers0, Options) ->
    Headers = hackney_headers:update(default_headers(Url), Headers0),
    FinalHeaders = case request_type(Headers) of
                       chunked ->
                           hackney_headers:store(<<"Transfer-Encoding">>, <<"chunked">>,
                                                 hackney_headers:delete(<<"Content-Length">>,
                                                                        Headers));
                       normal ->
                           Headers
                   end,

    maybe_add_cookies(proplists:get_value(cookie, Options), FinalHeaders, Options).

maybe_add_cookies(undefined, Headers, Options) ->
    maybe_add_auth(Headers, Options);
maybe_add_cookies(Cookies, Headers, Options) when is_list(Cookies) ->
    NewHeaders = lists:foldr(fun(Cookie, Hdrs) ->
                                     cookie_hdr(Cookie, Hdrs)
                             end, Headers, Cookies),
    maybe_add_auth(NewHeaders, Options);
maybe_add_cookies(Cookie, Headers, Options) ->
    NewHeaders = cookie_hdr(Cookie, Headers),
    maybe_add_auth(NewHeaders, Options).

cookie_hdr(Cookie, Headers) when is_binary(Cookie) ->
    hackney_headers:insert(<<"Cookie">>, Cookie, Headers);
cookie_hdr({Name, Value}, Headers) ->
    Cookie = hackney_cookie:setcookie(Name, Value, []),
    hackney_headers:insert(<<"Cookiee">>, Cookie, Headers);
cookie_hdr({Name, Value, Params}, Headers) ->
    Cookie = hackney_cookie:setcookie(Name, Value, Params),
    hackney_headers:insert(<<"Cookie">>, Cookie, Headers).


maybe_add_auth(Headers, Options) ->
    case proplists:get_value(basic_auth, Options) of
        undefined -> Headers;
        {User, Pwd} ->
            Creds = base64:encode(<< (hackney_bstr:to_binary(User))/binary, ":",
                                     (hackney_bstr:to_binary(Pwd))/binary >>),
            hackney_headers:store(<<"Authorization">>, << "Basic ", Creds/binary >>,
                                  Headers)
    end.


request_method(Method) ->
    hackney_bstr:to_upper(hackney_bstr:to_binary(Method)).

request_path(<<"CONNECT">>, Url) ->
    #hackney_url{host=Host, port=Port} = Url,
    iolist_to_binary([Host, ":", Port]);
request_path(_Method, Url) ->
    #hackney_url{path=Path, qs=Query} = Url,
    case Query of
        <<>> -> Path;
        _ -> << Path/binary, "?", Query/binary >>
    end.

default_ua() ->
    Version = case application:get_key(hackney, vsn) of
                  {ok, FullVersion} ->
                      list_to_binary(hd(string:tokens(FullVersion, "-")));
                  _ ->
                      <<"0.0.0">>
              end,
    <<"hackney/", Version/binary>>.

default_headers(Url) ->
    #hackney_url{netloc=Netloc} = Url,
    UA = default_ua(),
    hackney_headers:new([{<<"Host">>, Netloc},
                         {<<"User-Agent">>, UA}]).


send_fun(chunked) ->
    fun send_chunk/2;
send_fun(_) ->
    fun hackney_socket:send/2.

send_chunk(HS, Data) ->
    Len = iolist_size(Data),
    hackney_socket:send(HS, [iolib:format("~.16b\r\b", [Len]), Data,
                             <<"\r\n">>]).
