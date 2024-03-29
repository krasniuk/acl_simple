acl_simple
=====

Build
-----
    $ make rel
    $ ./_build/prod/rel/acl_simple/bin/acl_simple console
    "or"
    $ ./run.sh

Common tests build
----
    $ rebar3 ct --spec apps/acl_simple/tests/common_tests/ct.spec

WRK
----
    $ wrk -t2 -c3 -d1m -R5700 -s apps/acl_simple/wrk/show_all_users.lua http://127.0.0.1:1913
    
Data developer
---- 
    Transmission base: http, POST.
    Transmission format: json.

    Roles: "read", "write", "exec".

    Cache saves in ets. Table 'acl_simple' in key 'server_cache'. 

API
-----
    acl_simple_server:
        State = #{<<"User">> => [<<"Role">>, ..], ..},
    
        handle_call:                                                                Have cache     Benchmark(wrk2)   Delay(wrk2)
            user_add({user_add, <<"NewUser">>}),                          (127 ms) | (106 ms)             (26 r/s) | (437 ms)
            user_delete({user_delete, <<"User">>}),                       (271 ms) | (297 ms)                      |
            show_all_users({show_all_users}),                             (114 ms) | (2 ms)            (15200 r/s) | (19 ms)
        
            roles_add({roles_add, <<"User">>, [<<"Role">>, ...]}),        (459 ms) | (246 ms)             (95 r/s) | (437 ms)
            roles_delete({roles_delete, <<"User">>, [<<"Role">>, ...]}),  (185 ms) | (208 ms)                      |
            show_roles({show_roles, <<"User">>}),                         (185 ms) | (2 ms)                        |
        
            show_allow_roles({show_allow_roles}).                         (1 ms)   | (1 ms)            (52000 r/s) | (17 ms)

        
JSON requests example
-----
    {"method":"show_allow_roles"}

    {"method":"show_all_users"}
    {"method":"user_delete", "user":"mike_test"}
    {"method":"user_add", "user":"mike_test"}
    
    {"method":"roles_add", "user":"karl_test", "roles":["read"]}
    {"method":"show_roles", "user":"karl_test"}
    {"method":"roles_delete", "user":"karl_test", "roles":["exec"]}

    