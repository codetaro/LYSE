{application, processquest, [
  {description, "Game inspired by the Progress Quest game (http://progressquest.com)"},
  {vsn, "1.1.0"},
  {registered, [pq_supersup]},
  {applications, [
    kernel,
    stdlib,
    regis,
    crypto
  ]},
  {modules, [processquest, pq_stats, pq_enemy, pq_events, pq_player]},
  {mod, {processquest, []}},
  {env, []}
]}.