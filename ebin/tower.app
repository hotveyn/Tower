{application, tower,
 [{description, "A simple caching system"},
  {vsn, "0.1.0"},
  {modules, [
             tower_gen,
             tower_sup,
             tower_store,
             tower_tcp_gen,
             tower_tcp_sup
            ]},
  {registered, [tower_sup, tower_tcp_sup]},
  {applications, [kernel, stdlib]},
  {mod, {tower_app, []}}
 ]}.