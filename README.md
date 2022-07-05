
# erlaws: AWS client for erlang #

An AWS client based on `erlaws3` utilizing AWS Signature v4 for doing simple HTTP API requests to Amazon AWS

## Config ##

Make sure to define your access/secret keys in the AWS config

```erlang
{erlaws, [
  %% AWS keys
  {access_key, <<>>},
  {secret_key, <<>>},
  {default_region, <<>>},
]}
```

## Usage ##

Sample Values

- Host: `<<"polly.ap-southeast-1.amazonaws.com">>`
- Method: `post | put | get | delete`
- CanonicalUri: `<<"/v1/speech">>`
- CanonicalQueryString: `<<"prefix=somePrefix&marker=someMarker&max-keys=20">>`
- Payload: `<<"{"hello": "world"}">>`
- Scope: `<<"polly">>`
- Headers: `[{<<"content-type">>, <<"application/json">>}]`

```erlang
erlaws:post(<<"/v1/speech">>, <<"{\"OutputFormat\": \"mp3\",\"Text\":\"hello\",\"VoiceId\":\"Amy\"}">>, <<"polly">>, []).
{ok,#{body =>
          <<73,68,51,4,0,0,0,0,0,35,84,83,83,69,0,0,0,15,0,0,3,76,
            97,118,102,53,...>>,
      headers =>
          [{<<"x-amzn-RequestId">>,
            <<"4aef1971-3ffc-43d1-b41b-d649eff18d7c">>},
           {<<"x-amzn-RequestCharacters">>,<<"5">>},
           {<<"Content-Type">>,<<"audio/mpeg">>},
           {<<"Transfer-Encoding">>,<<"chunked">>},
           {<<"Date">>,<<"Tue, 05 Jul 2022 03:23:30 GMT">>}],
      status_code => 200}}
```
