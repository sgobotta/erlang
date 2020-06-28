{application, calc,
  [{description,  "Calc application for the Erlang master classes"},
   {id,           "420"},
   {vsn,          "1.0"},
   {modules,      [calc, calc_sup, calc_app, expr]},
   {registered,   [calc_sup, calc]},
   {applications, [kernel, stdlib]},
   {env,          [{env, [{a,23},{b,-12}]}]},
   {mod,          {calc_app,[]}}]}.
