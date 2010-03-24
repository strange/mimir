{application, mimir,
 [{description, "mimir"},
  {vsn, "0.01"},
  {modules, [
    mimir,
    mimir_web,
    mimir_app,
    mimir_sup
  ]},
  {registered, []},
  {mod, {mimir_app, []}},
  {env, []},
  {applications, [inets, kernel, stdlib, crypto]}]}.
