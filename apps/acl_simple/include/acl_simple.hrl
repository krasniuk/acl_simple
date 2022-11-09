%% LAGER MACROS

-define(LOG_DEBUG(Format, Args),    lager:log(debug,    self(), Format, Args)).
-define(LOG_INFO(Format, Args),     lager:log(info,     self(), Format, Args)).
-define(LOG_WARNING(Format, Args),  lager:log(warning,  self(), Format, Args)).
-define(LOG_ERROR(Format, Args),    lager:log(error,    self(), Format, Args)).
-define(LOG_CRITICAL(Format, Args), lager:log(critical, self(), Format, Args)).
