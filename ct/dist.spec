{node, a, 'a@DENNIS-RAZER'}.
{node, b, 'b@DENNIS-RAZER'}.

{init, [a,b], [{node_start, [{monitor_master, true}]}]}.

{alias, demo, "./demo/"}.
{alias, meeting, "./meeting/"}.

{logdir, all_nodes, "./logs/"}.
{logdir, master, "./logs/"}.

{suites, [b], meeting, all}.
{suites, [a], demo, all}.
{skip_cases, [a], demo, basic_SUITE, test2, "This test fails on purpose"}.