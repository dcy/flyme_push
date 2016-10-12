# flyme_push

## 通知栏
    通知栏主要是两个字段Title和Content

### notification
    根据PushIds推送通知栏消息

* notification(PushIds, Title, Content):
```erlang
flyme_push:notification("UU34b4f75595d58540a78407f4d5a60630642497c5c5e", "Hello", "World").
```

* notification(AppId, AppSecret, PushIds, Title, Content):
```erlang
flyme_push:notification(123456, "FlymeAppSecret", "UU34b4f75595d58540a78407f4d5a60630642497c5c5e", "Hello", "World").
```

### unvarnished
    根据PushIds推送透传消息


* unvarnished(PushIds, Content)
```erlang
flyme_push:unvarnished("UU34b4f75595d58540a78407f4d5a60630642497c5c5e", jiffy:encode(#{<<"hello">> => <<"world">>})).
```

* unvarnished(AppId, AppSecret, PushIds, Content)
```erlang
flyme_push:unvarnished(123456, "FlymeAppSecret", "UU34b4f75595d58540a78407f4d5a60630642497c5c5e", jiffy:encode(#{<<"hello">> => <<"world">>})).
```


