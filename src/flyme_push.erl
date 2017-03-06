-module(flyme_push).

%% API
-export([varnished/1, varnished/3, varnished/5,
         varnished_tags/1, varnished_tags/4, varnished_tags/6,
         varnished_all/1, varnished_all/2, varnished_all/4,
         unvarnished/1, unvarnished/2, unvarnished/4,
         unvarnished_tags/1, unvarnished_tags/3, unvarnished_tags/5,
         unvarnished_all/1, unvarnished_all/3,

         general_notification/5, general_app_msg/4,

         notification/1, notification/3, notification/5,
         notification_tags/1, notification_tags/4, notification_tags/6,
         notification_all/1, notification_all/2, notification_all/4
        ]).

-export([send/4]).

-include_lib("eutil/include/eutil.hrl").

-define(HEADERS, [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}]).

varnished(PayloadMaps) ->
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
    do_send(URL, PayloadMaps).

notification(PayloadMaps) ->
    varnished(PayloadMaps).

varnished(AppId, AppSecret, PayloadMaps) when is_map(PayloadMaps) ->
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
    send(AppId, AppSecret, URL, PayloadMaps);
varnished(PushIds, Title, Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    varnished(AppId, AppSecret, PushIds, Title, Content).

notification(PushIds, Title, Content) ->
    varnished(PushIds, Title, Content).

notification(AppId, AppSecret, PushIds, Title, Content) ->
    varnished(AppId, AppSecret, PushIds, Title, Content).

varnished(AppId, AppSecret, PushIds, Title, Content) ->
    NoticeBarInfo = #{<<"noticeBarType">> => 0,
                      <<"title">> => unicode:characters_to_binary(Title),
                      <<"content">> => unicode:characters_to_binary(Content)},
    MessageJson = jiffy:encode(#{<<"noticeBarInfo">> => NoticeBarInfo}),
    PayloadMaps = #{<<"pushIds">> => eutil:to_binary(PushIds),
                    <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
    send(AppId, AppSecret, URL, PayloadMaps).



varnished_tags(PayloadMaps) ->
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/pushTask/pushToTag">>,
    do_send(URL, PayloadMaps).

notification_tags(PayloadMaps) ->
    varnished_tags(PayloadMaps).

varnished_tags(Tags, Scope, Title, Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    varnished_tags(AppId, AppSecret, Tags, Scope, Title, Content).

%% Scope 0并集 1交集
notification_tags(Tags, Scope, Title, Content) ->
    varnished_tags(Tags, Scope, Title, Content).


notification_tags(AppId, AppSecret, Tags, Scope, Title, Content) ->
    varnished_tags(AppId, AppSecret, Tags, Scope, Title, Content).

varnished_tags(AppId, AppSecret, Tags, Scope, Title, Content) ->
    NoticeBarInfo = #{<<"title">> => unicode:characters_to_binary(Title),
                      <<"content">> => unicode:characters_to_binary(Content)},
    MessageJson = jiffy:encode(#{<<"noticeBarInfo">> => NoticeBarInfo}),
    PayloadMaps = #{<<"pushType">> => 0,
                    <<"tagNames">> => Tags,
                    <<"scope">> => Scope,
                    <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/pushTask/pushToTag">>,
    send(AppId, AppSecret, URL, PayloadMaps).

varnished_all(PayloadMaps) ->
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/pushTask/pushToApp">>,
    do_send(URL, PayloadMaps).

notification_all(PayloadMaps) ->
    varnished_all(PayloadMaps).

notification_all(Title, Content) ->
    varnished_all(Title, Content).

varnished_all(Title, Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    varnished_all(AppId, AppSecret, Title, Content).

notification_all(AppId, AppSecret, Title, Content) ->
    varnished_all(AppId, AppSecret, Title, Content). 

varnished_all(AppId, AppSecret, Title, Content) ->
    NoticeBarInfo = #{<<"title">> => unicode:characters_to_binary(Title),
                      <<"content">> => unicode:characters_to_binary(Content)},
    MessageJson = jiffy:encode(#{<<"noticeBarInfo">> => NoticeBarInfo}),
    PayloadMaps = #{<<"pushType">> => 0, <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/pushTask/pushToApp">>,
    send(AppId, AppSecret, URL, PayloadMaps).

unvarnished(PayloadMaps) ->
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
    do_send(URL, PayloadMaps).

unvarnished(PushIds, Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    unvarnished(AppId, AppSecret, PushIds, Content).

unvarnished(AppId, AppSecret, PushIds, Content) ->
    MessageJson = jiffy:encode(#{<<"content">> => Content}),
    PayloadMaps = #{<<"pushIds">> => eutil:to_binary(PushIds),
                    <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
    send(AppId, AppSecret, URL, PayloadMaps).

unvarnished_tags(PayloadMaps) ->
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/pushTask/pushToTag">>,
    do_send(URL, PayloadMaps).

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

unvarnished_all(Maps) when is_map(Maps) ->
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/pushTask/pushToApp">>,
    do_send(URL, Maps);
unvarnished_all(Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    unvarnished_all(AppId, AppSecret, Content).

unvarnished_all(AppId, AppSecret, Content) ->
    MessageJson = jiffy:encode(#{<<"title">> => <<"unvarnished_all">>, <<"content">> => Content}),
    PayloadMaps = #{<<"pushType">> => 1, <<"messageJson">> => MessageJson},
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/pushTask/pushToApp">>,
    send(AppId, AppSecret, URL, PayloadMaps).


gen_sign(Maps, AppSecret) ->
    Fun = fun({K, V}, TempStr) ->
                  TempStr ++ eutil:to_list(K) ++ "=" ++ eutil:to_list(V)
          end,
    KvStr = lists:foldl(Fun, "", lists:sort(maps:to_list(Maps))),
    eutil:md5_hex(KvStr ++ AppSecret).

send(AppId, AppSecret, URL, PayloadMaps) ->
    AppIdMaps = PayloadMaps#{<<"appId">> => AppId},
    Sign = gen_sign(AppIdMaps, AppSecret),
    SignMaps = AppIdMaps#{<<"sign">> => Sign},
    do_send(URL, SignMaps).

do_send(URL, PayloadMaps) ->
    Payload = eutil:urlencode(PayloadMaps),
    Method = post,
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
            lager:error("flyme_push error, PayloadMaps:~p, Result:~p", [PayloadMaps, Result]),
            ok
    end.


general_notification(AppId, AppSecret, PushIds, Title, Content) ->
    varnished(AppId, AppSecret, PushIds, Title, Content).

general_app_msg(AppId, AppSecret, PushIds, Content) ->
    unvarnished(AppId, AppSecret, PushIds, Content).
