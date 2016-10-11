flyme_push
=====

```erlang
flyme_push:notification(123456, "FlymeAppSecret", "UU34b4f75595d58540a78407f4d5a60630642497c5c5e", "Hello", "World").
flyme_push:unvarnished(123456, "FlymeAppSecret", "UU34b4f75595d58540a78407f4d5a60630642497c5c5e", jiffy:encode(#{<<"hello">> => <<"world">>})).
```
