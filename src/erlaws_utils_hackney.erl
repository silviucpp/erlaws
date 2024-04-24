-module(erlaws_utils_hackney).

-include("erlaws.hrl").

-export([
    http_request/7,
    http_request/8,
    http_request/9
]).

http_request(Url, Method, Headers, Options, Payload, RetryCount, ExpectedCodes) ->
    http_request(Url, Method, Headers, Options, Payload, RetryCount, ExpectedCodes, null, infinity).

http_request(Url, Method, Headers, Options, Payload, RetryCount, ExpectedCodes, LogFun) ->
    http_request(Url, Method, Headers, Options, Payload, RetryCount, ExpectedCodes, LogFun, infinity).

http_request(Url, Method, Headers, Options, Payload, RetryCount, ExpectedCodes, LogFun, MaxLengthBody) ->

    try

        {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(Method, Url, Headers, Payload, Options),

        {ok, Body} = hackney:body(ClientRef, MaxLengthBody),

        case should_retry(StatusCode, ExpectedCodes, RetryCount) of
            true ->
                run_log_function(LogFun, {status_code, StatusCode, Body}),
                throw({bad_response, StatusCode});
            _ ->
                {ok, StatusCode, RespHeaders, Body}
        end

    catch
        ?EXCEPTION(_, Term, Stacktrace) ->
            case Term of
                {bad_response, Status} ->
                    ?ERROR_MSG("http_request unexpected result url: ~p status: ~p payload: ~p retry: ~p", [Url, Status, Payload, RetryCount]),
                    http_request(Url, Method, Headers, Options, Payload, RetryCount - 1, ExpectedCodes, LogFun, MaxLengthBody);
                _ ->
                    % workaround for hackney bug. todo: remove this once hackney get fixed
                    CanRetry = case Term of
                        {badmatch,{error, checkout_timeout}} ->
                            case erlaws_utils:lookup(pool, Options) of
                                null ->
                                    ok;
                                Pool ->
                                    ?ERROR_MSG("http_request url: ~p checkout timeout ... Restart pool ~p.", [Url, Pool]),
                                    PoolMaxConnections = hackney_pool:max_connections(Pool),
                                    PoolTimeout = hackney_pool:timeout(Pool),
                                    hackney_pool:stop_pool(Pool),
                                    hackney_pool:start_pool(Pool, [{timeout, PoolTimeout}, {max_connections, PoolMaxConnections}])
                           end,
                           true;
                        {badmatch,{error,nxdomain}} ->
                            false;
                        _ ->
                            true
                    end,

                    ?ERROR_MSG("http_request url: ~p exception: ~p stack: ~p payload: ~p retry times: ~p can_retry: ~p", [Url, Term, ?GET_STACK(Stacktrace), Payload, RetryCount, CanRetry]),
                    run_log_function(LogFun, Term),

                    case CanRetry andalso RetryCount > 0 of
                        true ->
                            http_request(Url, Method, Headers, Options, Payload, RetryCount - 1, ExpectedCodes, LogFun, MaxLengthBody);
                        _ ->
                            {error, Term}
                    end
            end
    end.

% internals

should_retry(StatusCode, ExpectedCodes, RetryCount) ->
    case RetryCount > 0 of
        true ->
            case ExpectedCodes of
                [] ->
                    false;
                _ ->
                    lists:member(StatusCode, ExpectedCodes) =:= false
            end;
        _ ->
            false
    end.

run_log_function(null, _) ->
    ok;
run_log_function(Fun, Error) ->
    try
        Fun(Error)
    catch _: Term ->
        ?ERROR_MSG("failed to send the log: ~p", [Term])
    end.
