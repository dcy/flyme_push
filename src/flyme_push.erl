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
         notification_all/1, notification_all/2, notification_all/4,

         get_register_switch/1, get_register_switch/3,
         change_register_switch/3, change_register_switch/5,
         change_all_switch/2, change_all_switch/4,
         subscribe_tags/2, subscribe_tags/4,
         unsubscribe_tags/2, unsubscribe_tags/4,
         get_sub_tags/1, get_sub_tags/3,
         unsub_all_tags/1, unsub_all_tags/3
        ]).

-export([send/4]).

-include_lib("eutil/include/eutil.hrl").

-define(HEADERS, [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}]).

varnished(PayloadMaps) ->
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
    do_send(URL, PayloadMaps).

notification(PayloadMaps) ->
    varnished(PayloadMaps).

varnished(AppId, AppSecret, PayloadMaps) when is_map(PayloadMaps) ->
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
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
    MessageJson = eutil:json_encode(#{<<"noticeBarInfo">> => NoticeBarInfo}),
    PayloadMaps = #{<<"pushIds">> => eutil:to_binary(PushIds),
                    <<"messageJson">> => MessageJson},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
    send(AppId, AppSecret, URL, PayloadMaps).



varnished_tags(PayloadMaps) ->
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/pushTask/pushToTag">>,
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
    MessageJson = eutil:json_encode(#{<<"noticeBarInfo">> => NoticeBarInfo}),
    PayloadMaps = #{<<"pushType">> => 0,
                    <<"tagNames">> => Tags,
                    <<"scope">> => Scope,
                    <<"messageJson">> => MessageJson},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/pushTask/pushToTag">>,
    send(AppId, AppSecret, URL, PayloadMaps).

varnished_all(PayloadMaps) ->
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/pushTask/pushToApp">>,
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
    MessageJson = eutil:json_encode(#{<<"noticeBarInfo">> => NoticeBarInfo}),
    PayloadMaps = #{<<"pushType">> => 0, <<"messageJson">> => MessageJson},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/pushTask/pushToApp">>,
    send(AppId, AppSecret, URL, PayloadMaps).

unvarnished(PayloadMaps) ->
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
    do_send(URL, PayloadMaps).

unvarnished(PushIds, Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    unvarnished(AppId, AppSecret, PushIds, Content).

unvarnished(AppId, AppSecret, PushIds, Content) ->
    MessageJson = eutil:json_encode(#{<<"content">> => Content}),
    PayloadMaps = #{<<"pushIds">> => eutil:to_binary(PushIds),
                    <<"messageJson">> => MessageJson},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
    send(AppId, AppSecret, URL, PayloadMaps).

unvarnished_tags(PayloadMaps) ->
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/pushTask/pushToTag">>,
    do_send(URL, PayloadMaps).

unvarnished_tags(Tags, Scope, Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    unvarnished_tags(AppId, AppSecret, Tags, Scope, Content).

unvarnished_tags(AppId, AppSecret, Tags, Scope, Content) ->
    MessageJson = eutil:json_encode(#{<<"content">> => Content}),
    PayloadMaps = #{<<"pushType">> => 1,
                    <<"tagNames">> => Tags,
                    <<"scope">> => Scope,
                    <<"messageJson">> => MessageJson},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/pushTask/pushToTag">>,
    send(AppId, AppSecret, URL, PayloadMaps).

unvarnished_all(Maps) when is_map(Maps) ->
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/pushTask/pushToApp">>,
    do_send(URL, Maps);
unvarnished_all(Content) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    unvarnished_all(AppId, AppSecret, Content).

unvarnished_all(AppId, AppSecret, Content) ->
    MessageJson = eutil:json_encode(#{<<"title">> => <<"unvarnished_all">>, <<"content">> => Content}),
    PayloadMaps = #{<<"pushType">> => 1, <<"messageJson">> => MessageJson},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/push/pushTask/pushToApp">>,
    send(AppId, AppSecret, URL, PayloadMaps).

get_register_switch(PushId) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    get_register_switch(AppId, AppSecret, PushId).

get_register_switch(AppId, AppSecret, PushId) ->
    PayloadMaps = #{<<"pushId">> => eutil:to_binary(PushId)},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/message/getRegisterSwitch">>,
    http_req(AppId, AppSecret, URL, PayloadMaps).

change_register_switch(PushId, MsgType, SubSwitch) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    change_register_switch(AppId, AppSecret, PushId, MsgType, SubSwitch).

change_register_switch(AppId, AppSecret, PushId, MsgType, SubSwitch) ->
    PayloadMaps = #{<<"pushId">> => eutil:to_binary(PushId), <<"msgType">> => MsgType,
                    <<"subSwitch">> => SubSwitch},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/message/changeRegisterSwitch">>,
    http_req(AppId, AppSecret, URL, PayloadMaps).

change_all_switch(PushId, SubSwitch) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    change_all_switch(AppId, AppSecret, PushId, SubSwitch).

change_all_switch(AppId, AppSecret, PushId, SubSwitch) ->
    PayloadMaps = #{<<"pushId">> => PushId, <<"subSwitch">> => SubSwitch},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/message/changeAllSwitch">>,
    http_req(AppId, AppSecret, URL, PayloadMaps).

subscribe_tags(PushId, Tags) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    subscribe_tags(AppId, AppSecret, PushId, Tags).

subscribe_tags(AppId, AppSecret, PushId, Tags) ->
    PayloadMaps = #{<<"pushId">> => PushId, <<"tags">> => Tags},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/message/subscribeTags">>,
    http_req(AppId, AppSecret, URL, PayloadMaps).

unsubscribe_tags(PushId, Tags) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    unsubscribe_tags(AppId, AppSecret, PushId, Tags).

unsubscribe_tags(AppId, AppSecret, PushId, Tags) ->
    PayloadMaps = #{<<"pushId">> => PushId, <<"tags">> => Tags},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/message/unSubscribeTags">>,
    http_req(AppId, AppSecret, URL, PayloadMaps).

unsub_all_tags(PushId) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    unsub_all_tags(AppId, AppSecret, PushId).

unsub_all_tags(AppId, AppSecret, PushId) ->
    PayloadMaps = #{<<"pushId">> => PushId},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/message/unSubAllTags">>,
    http_req(AppId, AppSecret, URL, PayloadMaps).


get_sub_tags(PushId) ->
    {ok, AppId} = application:get_env(flyme_push, app_id),
    {ok, AppSecret} = application:get_env(flyme_push, app_secret),
    get_sub_tags(AppId, AppSecret, PushId).

get_sub_tags(AppId, AppSecret, PushId) ->
    PayloadMaps = #{<<"pushId">> => PushId},
    URL = <<"http://server-api-push.meizu.com/garcia/api/server/message/getSubTags">>,
    http_req(AppId, AppSecret, URL, PayloadMaps).











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
    Result = eutil:json_decode(ResultBin),
    %%todo: 1003服务器忙要不要返回队列
    #{<<"code">> := Code, <<"value">> := Value} = Result,
    case Code of
        <<"200">> ->
            case Value == #{} of
                true ->
                    {ok, Result};
                false ->
                    error_logger:error_msg("flyme_push error, PayloadMaps:~p, Result:~p", [PayloadMaps, Result]),
                    {push_id_illegal, Result}
            end;
        _ ->
            error_logger:error_msg("flyme_push error, PayloadMaps:~p, Result:~p", [PayloadMaps, Result]),
            {error, Result}
    end.

http_req(AppId, AppSecret, URL, PayloadMaps) ->
    AppIdMaps = PayloadMaps#{<<"appId">> => AppId},
    Sign = gen_sign(AppIdMaps, AppSecret),
    SignMaps = AppIdMaps#{<<"sign">> => Sign},
    do_http_req(URL, SignMaps).

do_http_req(URL, PayloadMaps) ->
    Payload = eutil:urlencode(PayloadMaps),
    Method = post,
    Options = [{pool, flyme}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, ?HEADERS,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    Result = eutil:json_decode(ResultBin),
    %%todo: 1003服务器忙要不要返回队列
    #{<<"code">> := Code, <<"value">> := Value} = Result,
    case Code of
        <<"200">> ->
            {ok, Result};
        _ ->
            error_logger:error_msg("flyme_push error, PayloadMaps:~p, Result:~p", [PayloadMaps, Result]),
            {error, Result}
    end.




general_notification(AppId, AppSecret, PushIds, Title, Content) ->
    varnished(AppId, AppSecret, PushIds, Title, Content).

general_app_msg(AppId, AppSecret, PushIds, Content) ->
    unvarnished(AppId, AppSecret, PushIds, Content).
