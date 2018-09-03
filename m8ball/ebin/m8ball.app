{application, m8ball, [
  {description, "Answer vital questions"},
  {vsn, "1.0.0"},
  {registered, [m8ball, m8ball_sup, m8ball_server]},
  {applications, [
    kernel,
    stdlib,
    crypto
  ]},
  {mod, {m8ball, []}},
  {env, [
    {answers, {
      <<"Yes">>, <<"No">>, <<"Doubtful">>,
      <<"I don't like your tone">>, <<"Of course">>,
      <<"Of course not">>, <<"*backs away slowly and runs away*">>}}
  ]}
]}.