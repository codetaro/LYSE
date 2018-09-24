{application, mafiapp, [
  {description, "Help the boss keep track of his friends"},
  {vsn, "1.0.0"},
  {modules, [mafiapp, mafiapp_sup]},
  {registered, []},
  {applications, [
    kernel,
    stdlib,
    mnesia
  ]},
  {mod, {mafiapp, []}},
  {env, []}
]}.