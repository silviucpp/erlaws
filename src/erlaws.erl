%%%-------------------------------------------------------------------
%% @doc Generic AWS client that handles header signature v4
%% @author Byron Wang <byronpc1@gmail.com>
%%
%% Example values:
%% Host: <<"polly.ap-southeast-1.amazonaws.com">>
%% Method: post | put | get | delete
%% CanonicalUri: <<"/v1/speech">>
%% CanonicalQueryString: <<"prefix=somePrefix&marker=someMarker&max-keys=20">>
%% Payload: <<"{...}">>
%% Scope: <<"polly">>
%% Headers: [{<<"content-type">>, <<"application/json">>}]
%% @end
%%%-------------------------------------------------------------------
-module(erlaws).
-export([get/4, delete/4, post/4, put/4, request/6, request/7, request/8]).

put(CanonicalUri, Payload, Scope, Headers) ->
  request(put, CanonicalUri, <<>>, Payload, Scope, Headers).

post(CanonicalUri, Payload, Scope, Headers) ->
  request(post, CanonicalUri, <<>>, Payload, Scope, Headers).

get(CanonicalUri, CanonicalQueryString, Scope, Headers) ->
  request(get, CanonicalUri, CanonicalQueryString, <<>>, Scope, Headers).

delete(CanonicalUri, CanonicalQueryString, Scope, Headers) ->
  request(delete, CanonicalUri, CanonicalQueryString, <<>>, Scope, Headers).

request(Method, CanonicalUri, CanonicalQueryString, Payload, Scope, Headers) ->
  {ok, Region} = application:get_env(erlaws, default_region),
  request(Method, CanonicalUri, CanonicalQueryString, Payload, Region, Scope, Headers).

request(Method, CanonicalUri, CanonicalQueryString, Payload, Region, Scope, Headers) ->
  Host = host(Scope, Region),
  request(Host, Method, CanonicalUri, CanonicalQueryString, Payload, Region, Scope, Headers).

request(Host, Method, CanonicalUri, CanonicalQueryString, Payload, Region, Scope, Headers) ->
  {ok, ConnPid} = erlaws_utils:http_open(Host, 443),
  Headers2 = erlaws_headers:generate(Host, Method, CanonicalUri, CanonicalQueryString, Payload, Region, Scope, Headers),
  erlaws_utils:http_request(ConnPid, Method, CanonicalUri, Headers2, Payload).

% internals

% https://docs.aws.amazon.com/general/latest/gr/rande.html

host(<<"cloudfront">>, _Region) ->
  <<"cloudfront.amazonaws.com">>;
host(<<"globalaccelerator">>, _Region) ->
  <<"globalaccelerator.amazonaws.com">>;
host(<<"iam">>, _Region) ->
  <<"iam.amazonaws.com">>;
host(<<"route53">>, _Region) ->
  <<"route53.amazonaws.com">>;
host(<<"waf">>, _Region) ->
  <<"waf.amazonaws.com">>;
host(Scope, Region) ->
  <<Scope/binary, ".", Region/binary, ".amazonaws.com">>.
