# flyme_push
> 魅族Flyme推送push server sdk for Erlang    
> 集成版本：https://github.com/dcy/epush

## 通知栏
    通知栏主要是两个字段Title和Content

### notification: 根据PushIds推送通知栏消息

* notification(PushIds, Title, Content):
```erlang
flyme_push:notification("PushIds", "Hello", "World").
```

* notification(AppId, AppSecret, PushIds, Title, Content):
```erlang
flyme_push:notification(123456, "AppSecret", "PushIds", "Hello", "World").
```

### notification_tags: 根据tags推送通知栏消息

* notification_tags(Tags, Scope, Title, Content)
    Scope 0并集 1交集
```erlang
flyme_push:notification_tags("tag1,tag2", 0, "hello", "world").
```

* notification_tags(AppId, AppSecret, Tags, Scope, Title, Content)
```erlang
flyme_push:notification_tags(123456, "AppSecret", "tag1,tag2", 0, "hello", "world").
```

### notification_all: 推送全部用户通知栏消息

* notification_all(Title, Content)
```erlang
flyme_push:notification_all("hello", "world").
```

* notification_all(AppId, AppSecret, Title, Content)
```erlang
flyme_push:notification_all(123456, "AppSecret", "hello", "world").
```

## 透传
    主要字段Content

### unvarnished: 根据PushIds推送透传消息

* unvarnished(PushIds, Content)
```erlang
flyme_push:unvarnished("PushIds", jiffy:encode(#{<<"hello">> => <<"world">>})).
```
* unvarnished(AppId, AppSecret, PushIds, Content)
```erlang
flyme_push:unvarnished(123456, "AppSecret", "PushIds", jiffy:encode(#{<<"hello">> => <<"world">>})).
```

### unvarnished_tags: 根据tags推送透传信息

* unvarnished_tags(Tags, Scope, Content)
```erlang
flyme_push:unvarnished_tags("tag1,tag2", 0, jiffy:encode(#{<<"hello">> => <<"world">>})).
```
* unvarnished_tags(AppId, AppSecret, Tags, Scope, Content)
```erlang
flyme_push:unvarnished_tags(123456, "AppSecret", "tag1,tag2", 0, jiffy:encode(#{<<"hello">> => <<"world">>})).
```

### unvarnished_all: 推送全部用户透传消息

* unvarnished_all(Content)
```erlang
flyme_push:unvarnished_all(jiffy:encode(#{<<"hello">> => <<"world">>})).
```



