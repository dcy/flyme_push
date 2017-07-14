-module(flyme_push_example).
-compile(export_all).

%-define(DEVICE_TOKEN, "WXG575b0f4e0c535859727c5f44707008475e5d0d4003").
-define(DEVICE_TOKEN, "ULY6c59636d7f5b714b4e5f645279655d7f655f6f6d7f").
%-define(DEVICE_TOKEN, "wrong_device_token").

varnished() ->
    Maps = #{<<"appId">> => 123456,
             <<"messageJson">> => <<"{\"noticeBarInfo\":{\"title\":\"Hello\",\"noticeBarType\":0,\"content\":\"World\"}}">>,
             <<"pushIds">> => list_to_binary(?DEVICE_TOKEN),
             <<"sign">> => "varnished_sign"},
    flyme_push:varnished(Maps).

varnished3() ->
    flyme_push:varnished(?DEVICE_TOKEN, "Hello", "World").

varnished_tags() ->
    Maps = #{<<"appId">> => 123456,
             <<"messageJson">> => <<"{\"noticeBarInfo\":{\"title\":\"Hello\",\"content\":\"World\"}}">>,
             <<"pushType">> => 0,
             <<"scope">> => 0,
             <<"sign">> => "varnished_tags_sign",
             <<"tagNames">> => "tags1"},
    flyme_push:varnished_tags(Maps).

varnished_tags4() ->
    flyme_push:varnished_tags("tags1", 0, "Hello", "World").

varnished_all() ->
    Maps = #{<<"appId">> => 123456,
             <<"messageJson">> => <<123,34,110,111,116,105,99,101,66,97,114,73,110,
                                    102,111,34,58,123,34,116,105,116,108,101,34,58,
                                    34,72,101,108,108,111,228,189,160,229,165,189,
                                    34,44,34,99,111,110,116,101,110,116,34,58,34,
                                    87,111,114,108,100,228,184,150,231,149,140,34,
                                    125,125>>,
             <<"pushType">> => 0,
             <<"sign">> => "varnished_all_sign"},
    flyme_push:varnished_all(Maps).

varnished_all2() ->
    flyme_push:varnished_all("Hello你好", "World世界").

unvarnished() ->
    flyme_push:unvarnished(?DEVICE_TOKEN, eutil:json_encode(#{<<"Hello">> => <<"World">>})).

unvarnished_tags() ->
    flyme_push:unvarnished_tags("tags1", 0, <<"Hello World">>).

unvarnished_all() ->
    flyme_push:unvarnished_all(<<"Hello World!">>).

get_register_switch() ->
    flyme_push:get_register_switch(?DEVICE_TOKEN).

change_register_switch() ->
    flyme_push:change_register_switch(?DEVICE_TOKEN, 0, 1).

change_all_switch() ->
    flyme_push:change_all_switch(?DEVICE_TOKEN, 1).

get_sub_tags() ->
    flyme_push:get_sub_tags(?DEVICE_TOKEN).

subscribe_tags() ->
    flyme_push:subscribe_tags(?DEVICE_TOKEN, "test_tag").

unsubscribe_tags() ->
    flyme_push:unsubscribe_tags(?DEVICE_TOKEN, "test_tag").

notification_tags() ->
    flyme_push:notification_tags("test_tag", 1, <<"title">>, <<"content">>).

unsub_all_tags() ->
    flyme_push:unsub_all_tags(?DEVICE_TOKEN).

