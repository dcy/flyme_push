# flyme_push
> 魅族Flyme推送push server sdk for Erlang    
> 集成版本：https://github.com/dcy/epush    
> 使用例子：[/src/flyme_push_example.erl](/src/flyme_push_example.erl)

## 通知栏 varnished notification 两个是一样的
    通知栏主要是两个字段Title和Content

### varnished(notification): 根据PushIds推送通知栏消息

* varnished(PayloadMaps) ->
```erlang
    Maps = #{<<"appId">> => 123456,
             <<"messageJson">> => <<"{\"noticeBarInfo\":{\"title\":\"Hello\",\"noticeBarType\":0,\"content\":\"World\"}}">>,
             <<"pushIds">> => list_to_binary(?DEVICE_TOKEN),
             <<"sign">> => "varnished_sign"},
    flyme_push:varnished(Maps).
```
* varnished(PushIds, Title, Content):
* varnished(AppId, AppSecret, PayloadMaps) when is_map(PayloadMaps) ->
* varnished(AppId, AppSecret, PushIds, Title, Content):
```erlang
    flyme_push:notification("PushIds", "Hello", "World").
```

### varnished_tags(notification_tags): 根据tags推送通知栏消息
* varnished_tags(PayloadMaps) ->
* varnished_tags(Tags, Scope, Title, Content) ->
* varnished_tags(AppId, AppSecret, Tags, Scope, Title, Content) ->
    Scope 0并集 1交集
```erlang
    flyme_push:varnished_tags("tag1,tag2", 0, "hello", "world").
```

### varnished_all(notification_all): 推送全部用户通知栏消息

* varnished_all(PayloadMaps) ->
* varnished_all(Title, Content) ->
* varnished_all(AppId, AppSecret, Title, Content) ->
```erlang
    flyme_push:varnished_all("hello", "world").
```

## 透传
    主要字段Content

### unvarnished: 根据PushIds推送透传消息

* unvarnished(PayloadMaps) ->
* unvarnished(PushIds, Content)
* unvarnished(AppId, AppSecret, PushIds, Content) ->
```erlang
    flyme_push:unvarnished("PushIds", jiffy:encode(#{<<"hello">> => <<"world">>})).
```

### unvarnished_tags: 根据tags推送透传信息
* unvarnished_tags(PayloadMaps) ->
* unvarnished_tags(Tags, Scope, Content)
* unvarnished_tags(AppId, AppSecret, Tags, Scope, Content) ->
```erlang
    flyme_push:unvarnished_tags("tag1,tag2", 0, jiffy:encode(#{<<"hello">> => <<"world">>})).
```

### unvarnished_all: 推送全部用户透传消息
* unvarnished_all(Maps) when is_map(Maps) ->
* unvarnished_all(Content)
* unvarnished_all(AppId, AppSecret, Content) ->
```erlang
    flyme_push:unvarnished_all(jiffy:encode(#{<<"hello">> => <<"world">>})).
```
