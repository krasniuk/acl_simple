%% LAGER MACROS

-define(LOG_DEBUG(Format, Args),    lager:log(debug,    self(), Format, Args)).
-define(LOG_INFO(Format, Args),     lager:log(info,     self(), Format, Args)).
-define(LOG_WARNING(Format, Args),  lager:log(warning,  self(), Format, Args)).
-define(LOG_ERROR(Format, Args),    lager:log(error,    self(), Format, Args)).
-define(LOG_CRITICAL(Format, Args), lager:log(critical, self(), Format, Args)).

-define(ROLES, [<<"read">>, <<"write">>, <<"exec">>]).

-define(JSON_ERROR(Req),
  #{
    <<"result">> => <<"error">>,
    <<"discription">> => list_to_binary(Req)
  }).

-define(JSON_OK,
  #{
    <<"result">> => <<"ok">>
  }).

-define(JSON_ALL_ROLES,
  #{
    <<"result">> => <<"ok">>,
    <<"roles">> => [<<"read">>, <<"write">>, <<"exec">>]
  }).

-define(JSON_USERS(Users),
  #{
    <<"result">> => <<"ok">>,
    <<"users">> => Users
  }).

-define(JSON_ROLES_OF_USER(UserName, Roles),
  #{
    <<"result">> => <<"ok">>,
    <<"user">> => UserName,
    <<"roles">> => Roles
  }).

-define(JSON_DEVELOPMENT,
  #{
    <<"result">> => <<"develop now">>
  }).