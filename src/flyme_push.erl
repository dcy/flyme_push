-module(flyme_push).

%% API
-export([notification/3, notification/5,
         notification_tags/4, notification_tags/6,
         notification_all/2, notification_all/4,
         unvarnished/2, unvarnished/4,
         unvarnished_tags/3, unvarnished_tags/5,
         unvarnished_all/1, unvarnished_all/3
        ]).

-export([send/4]).

-define(HEADERS, [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}]).

notification(PushIds, Title, Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    notification(AppId, AppSecret, PushIds, Title, Content).

notification(AppId, AppSecret, PushIds, Title, Content) ->
    NoticeBarInfo = #{<<"noticeBarType">> => 0,
                      <<"title">> => unicode:characters_to_binary(Title),
                      <<"content">> => unicode:characters_to_binary(Content)},
    MessageJson = jiffy:encode(#{<<"noticeBarInfo">> => NoticeBarInfo}),
    PayloadMaps = #{<<"pushIds">> => list_to_binary(PushIds),
                    <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
    send(AppId, AppSecret, URL, PayloadMaps).

%% Scope 0并集 1交集
notification_tags(Tags, Scope, Title, Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    notification_tags(AppId, AppSecret, Tags, Scope, Title, Content).

notification_tags(AppId, AppSecret, Tags, Scope, Title, Content) ->
    NoticeBarInfo = #{<<"title">> => unicode:characters_to_binary(Title),
                      <<"content">> => unicode:characters_to_binary(Content)},
    MessageJson = jiffy:encode(#{<<"noticeBarInfo">> => NoticeBarInfo}),
    PayloadMaps = #{<<"pushType">> => 0,
                    <<"tagNames">> => Tags,
                    <<"scope">> => Scope,
                    <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/pushTask/pushToTag">>,
    send(AppId, AppSecret, URL, PayloadMaps).

notification_all(Title, Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    notification_all(AppId, AppSecret, Title, Content).

notification_all(AppId, AppSecret, Title, Content) ->
    NoticeBarInfo = #{<<"title">> => unicode:characters_to_binary(Title),
                      <<"content">> => unicode:characters_to_binary(Content)},
    MessageJson = jiffy:encode(#{<<"noticeBarInfo">> => NoticeBarInfo}),
    PayloadMaps = #{<<"pushType">> => 0, <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/pushTask/pushToApp">>,
    send(AppId, AppSecret, URL, PayloadMaps).

unvarnished(PushIds, Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    unvarnished(AppId, AppSecret, PushIds, Content).

unvarnished(AppId, AppSecret, PushIds, Content) ->
    MessageJson = jiffy:encode(#{<<"content">> => Content}),
    PayloadMaps = #{<<"pushIds">> => list_to_binary(PushIds),
                    <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
    send(AppId, AppSecret, URL, PayloadMaps).

unvarnished_tags(Tags, Scope, Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    unvarnished_tags(AppId, AppSecret, Tags, Scope, Content).

unvarnished_tags(AppId, AppSecret, Tags, Scope, Content) ->
    MessageJson = jiffy:encode(#{<<"content">> => Content}),
    PayloadMaps = #{<<"pushType">> => 1,
                    <<"tagNames">> => Tags,
                    <<"scope">> => Scope,
                    <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/pushTask/pushToTag">>,
    send(AppId, AppSecret, URL, PayloadMaps).

unvarnished_all(Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    unvarnished_all(AppId, AppSecret, Content).

unvarnished_all(AppId, AppSecret, Content) ->
    MessageJson = jiffy:encode(#{<<"content">> => Content}),
    PayloadMaps = #{<<"pushType">> => 1, <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/pushTask/pushToApp">>,
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
