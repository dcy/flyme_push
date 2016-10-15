-module(flyme_push_example).
-compile(export_all).

-define(DEVICE_TOKEN, "UU34b4f75595d58540a78407f4d5a60630642497c5c5e").

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
    flyme_push:unvarnished(?DEVICE_TOKEN, jiffy:encode(#{<<"Hello">> => <<"World">>})).

unvarnished_tags() ->
    flyme_push:unvarnished_tags("tags1", 0, <<"Hello World">>).

unvarnished_all() ->
    flyme_push:unvarnished_all(<<"Hello World!">>).
