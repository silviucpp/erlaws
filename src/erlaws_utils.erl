%%%-------------------------------------------------------------------
%% @doc Utilities
%% @author Byron Wang <byronpc1@gmail.com>
%% @end
%%%-------------------------------------------------------------------
-module(erlaws_utils).
-export([
  get_timestamp/0,
  sha256_to_hex/1,
  http_open/2,
  http_close/1,
  http_request/5,
  http_response/2
]).

-define(DEFAULT_CHUNK_SIZE, 1048576).

%%====================================================================
%% @doc Utilities
%%====================================================================
get_timestamp() ->
  {{Y,M,D},{H,Mi,S}}   = calendar:universal_time(),
  list_to_binary(io_lib:format("~4..0w~2..0w~2..0wT~2..0w~2..0w~2..0wZ", [Y,M,D,H,Mi,S])).

sha256_to_hex(<<Bin:256/big-unsigned-integer>>) ->
  list_to_binary(io_lib:format("~64.16.0b", [Bin])).

http_open(Url, Port) ->
  hackney:connect(hackney_ssl, Url, Port).

http_close(ConnPid) ->
  hackney:close(ConnPid).

http_request(ConnPid, Method, CanonicalUri, Headers, Payload) ->
  Response = hackney:send_request(ConnPid, {Method, CanonicalUri, Headers, Payload}),
  http_response(ConnPid, Response).

http_response(ConnPid, Response) ->
  case Response of
    {ok, StatusCode, RespHeaders, _Ref} ->
      {ok, Body} = hackney:body(ConnPid),
      http_close(ConnPid),
      {ok, #{status_code => StatusCode, headers => RespHeaders, body => Body}};
    Error ->
      Error
  end.

chunk_send_body(ConnPid, Fid, ChunkSize, Offset, ContentSize) ->
  NextContentSize = ContentSize - ChunkSize,
  NextOffset = Offset + ChunkSize,
  if NextContentSize < 1 ->
    {ok, Bytes} = file:pread(Fid, [{Offset, ContentSize}]),
    ok = hackney:send_body(ConnPid, Bytes);
  true ->
    {ok, Bytes} = file:pread(Fid, [{Offset, ChunkSize}]),
    ok = hackney:send_body(ConnPid, Bytes),
    chunk_send_body(ConnPid, Fid, ChunkSize, NextOffset, NextContentSize)
  end.
