-module(flyme_push).

%% API
-export([notification/5,
         unvarnished/4
        ]).

-export([send/4]).

-define(HEADERS, [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}]).

notification(AppId, AppSecret, PushIds, Title, Content) ->
    NoticeBarInfo = #{<<"noticeBarType">> => 0,
                      <<"title">> => unicode:characters_to_binary(Title),
                      <<"content">> => unicode:characters_to_binary(Content)},
    MessageJson = jiffy:encode(#{<<"noticeBarInfo">> => NoticeBarInfo}),
    PayloadMaps = #{<<"pushIds">> => list_to_binary(PushIds),
                    <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
    send(AppId, AppSecret, URL, PayloadMaps).

unvarnished(AppId, AppSecret, PushIds, Content) ->
    MessageJson = jiffy:encode(#{<<"content">> => Content}),
    PayloadMaps = #{<<"pushIds">> => list_to_binary(PushIds),
                    <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
    send(AppId, AppSecret, URL, PayloadMaps).






gen_sign(Maps, AppSecret) ->
    Fun = fun({K, V}, TempStr) ->
                  TempStr ++ to_list(K) ++ "=" ++ to_list(V)
          end,
    KvStr = lists:foldl(Fun, "", lists:sort(maps:to_list(Maps))),
    md5_hex(KvStr ++ AppSecret).

send(AppId, AppSecret, URL, PayloadMaps) ->
    AppIdMaps = PayloadMaps#{<<"appId">> => AppId},
    Sign = gen_sign(AppIdMaps, AppSecret),
    SignMaps = AppIdMaps#{<<"sign">> => Sign},
    Method = post,
    Payload = hackney_url:qs(maps:to_list(SignMaps)),
    Options = [{pool, flyme}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, ?HEADERS,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    Result = jiffy:decode(ResultBin, [return_maps]),
    %%todo: 1003服务器忙要不要返回队列
    case maps:get(<<"code">>, Result) of
        <<"200">> ->
            ok;
        _ ->
            lager:error("flyme_push error, PayloadMaps:~p, Result:~p", [SignMaps, Result]),
            ok
    end.

to_list(Item) when is_integer(Item) ->
    erlang:integer_to_list(Item);
to_list(Item) when is_binary(Item) ->
    erlang:binary_to_list(Item);
to_list(Item) when is_list(Item) ->
    Item;
to_list(Item) when is_atom(Item) ->
    erlang:atom_to_list(Item).


md5_hex(S) ->
	Md5_bin = erlang:md5(S),
	Md5_list = binary_to_list(Md5_bin),
	lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
	lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
	[hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
	$0 + N;
hex(N) when N >= 10, N < 16 ->
	$a + (N - 10).
