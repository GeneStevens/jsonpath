%% Logging

-include_lib("kernel/include/logger.hrl").

-define(DEBUG(Format, Args),
        ?LOG_DEBUG(Format, Args)).
-define(INFO(Format, Args),
        ?LOG_INFO(Format, Args)).
-define(WARN(Format, Args),
        ?LOG_WARN(Format, Args)).
-define(FATAL(Format, Args),
        ?LOG_FATAL(Format, Args)).
-define(ERROR(Format, Args),
        ?LOG_ERROR(Format, Args)).
