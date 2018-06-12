%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 12. Jun 2018 10:43 AM
%%%-------------------------------------------------------------------
{application, erlcount, [
  {description, ""},
  {vsn, "1.0.0"},
  {modules, [erlcount, erlcount_sup, erlcount_lib,
    erlcount_dispatch, erlcount_counter]},
  {registered, [erlcount]},
  {applications, [
    kernel,
    stdlib,
    ppool
  ]},
  {mod, {erlcount, []}},
  {env, [
    {directory, "."},
    {regex, ["if\\s.+->", "case\\s.+\\sof"]},
    {max_files, 10}]}
]}.