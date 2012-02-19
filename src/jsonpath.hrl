%% Logging
-define(DEBUG(Format, Args),
    lager:log(debug, self(), "[~p:~p] " ++ Format, [?MODULE, ?LINE | Args])).
-define(INFO(Format, Args),
    lager:log(info, self(), "[~p:~p] " ++ Format, [?MODULE, ?LINE | Args])).
-define(WARN(Format, Args),
    lager:log(warning, self(), "[~p:~p] " ++ Format, [?MODULE, ?LINE | Args])).
-define(FATAL(Format, Args),
    lager:log(critical, self(), "[~p:~p] " ++ Format, [?MODULE, ?LINE | Args])).
-define(ERROR(Format, Args),
    lager:log(error, self(), "[~p:~p] " ++ Format, [?MODULE, ?LINE | Args])).
