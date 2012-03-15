# JSONPath

JSONPath is an Erlang-based fast JSON data retrieval and update module.  It operates on binary data and uses [jiffy](/davisp/jiffy) for fast and efficient decoding of binary JSON into structured Erlang terms and back again.

## JSONPath is simple

For example, this simple JSON document:

```json
{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}
```

Can be searched with JavaScript-like notation in binary:

```erlang
(jsonpath@127.0.0.1)1> {ok, Data} = file:read_file("test.json").
{ok,<<"{\"menu\": {\n  \"id\": \"file\",\n  \"value\": \"File\",\n  \"popup\": {\n    \"menuitem\": [\n      {\"value\": \"New\", \"onclick"...>>}
(jsonpath@127.0.0.1)2> jsonpath:search(<<"menu.popup.menuitem[1].onclick">>, Data).
<<"OpenDoc()">>
(jsonpath@127.0.0.1)3>
```

## JSONPath is fast

Using an 80KB Foursquare API venue search document to read a very specific node deep in the document can produce a result like so:

```erlang
(jsonpath@127.0.0.1)4> jsonpath:search(<<"response.venues[6].categories[0].shortName">>, Data2).
<<"Sushi">>
(jsonpath@127.0.0.1)5>
```

Pre-decode the binary document into a structured Erlang term (via jiffy) and run that query one million times:

```erlang
(jsonpath@127.0.0.1)3> JiffyData = jiffy:decode(Data2).
{[{<<"meta">>,{[{<<"code">>,200}]}},
  {<<"notifications">>,
   [{[{<<"item">>,{[{<<"unreadCount">>,2}]}},
      {<<"type">>,<<"notificationTray">>}]}]},
      ...
(jsonpath@127.0.0.1)4> jsonpath_tests:bench(jsonpath, search, [<<"response.venues[6].categories[0].shortName">>, JiffyData], 1000000).
Range: 0 - 8853 mics
Median: 5 mics
Average: 5 mics
5
(jsonpath@127.0.0.1)5>
```

That's 5 microseconds on average on a MacBook Pro.

## JSONPath can alter your data

Using the same small JSON document from the first example, replace the content of a single node:

```erlang
(jsonpath@127.0.0.1)9> Json = jsonpath:replace(<<"menu.popup.menuitem[1].onclick">>, <<"NewFunction()">>, Data).
{[{<<"menu">>,
   {[{<<"id">>,<<"file">>},
     {<<"value">>,<<"File">>},
     {<<"popup">>,
      {[{<<"menuitem">>,
         [{[{<<"value">>,<<"New">>},
            {<<"onclick">>,<<"CreateNewDoc()">>}]},
          {[{<<"value">>,<<"Open">>},
            {<<"onclick">>,<<"NewFunction()">>}]},
          {[{<<"value">>,<<"Close">>},
            {<<"onclick">>,<<"CloseDoc()">>}]}]}]}}]}}]}
(jsonpath@127.0.0.1)10> NewData = jiffy:encode(Json).
<<"{\"menu\":{\"id\":\"file\",\"value\":\"File\",\"popup\":{\"menuitem\":[{\"value\":\"New\",\"onclick\":\"CreateNewDoc()\"},{\"value\":\"Open\","...>>
(jsonpath@127.0.0.1)11> 
```
