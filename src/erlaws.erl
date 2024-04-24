-include("erlaws.hrl").

-module(erlaws).

-export([
    get/4,
    delete/4,
    post/4,
    put/4,
    request/6,
    request/7,
    request/8
]).

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
    request(host(Scope, Region), Method, CanonicalUri, CanonicalQueryString, Payload, Region, Scope, Headers).

request(Host, Method, CanonicalUri, CanonicalQueryString, Payload, Region, Scope, Headers) ->
    Uri = <<"https://", Host/binary, "/", CanonicalUri/binary>>,
    Headers2 = erlaws_headers:generate(Host, Method, CanonicalUri, CanonicalQueryString, Payload, Region, Scope, Headers),
    Options = [{follow_redirect, true}, {pool, ?HACKNEY_POOL}, insecure],
    case erlaws_utils_hackney:http_request(Uri, Method, Headers2, Options, Payload, 0, []) of
        {ok, StatusCode, RespHeaders, Body} ->
            {ok, #{status_code => StatusCode, headers => RespHeaders, body => Body}};
        Error ->
            Error
    end.

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
