-module(ctrb_blackwhite_pubex_test).

-include_lib("eunit/include/eunit.hrl").


blackwhite_exclude_test() ->
    Id = 1234,
    Subs0 = [123, 234, 345, 456, 567],
    Subs = [234, 456],
    Options = #{exclude => [345, 123, 567, 890]},
    ?assertEqual(Subs,
                 ctrb_blackwhite_pubex:filter_subscriber(Subs0, Options, Id)),
    ok.

blackwhite_exclude_id_test() ->
    {ok, Mock} = start_mock(),
    try
        Id = 1234,
        Subs0 = [123, 234, 345, 456, 567],
        Subs = [123, 345, 567],
        Options = #{exclude_authid => [two_id, four_id]},
        ?assertEqual(Subs,
                     ctrb_blackwhite_pubex:filter_subscriber(Subs0, Options, Id))
    after
        ok = stop_mock(Mock)
    end,
    ok.

blackwhite_exclude_role_test() ->
    {ok, Mock} = start_mock(),
    try
        Id = 1234,
        Subs0 = [123, 234, 345, 456, 567],
        Subs = [123, 456, 567],
        Options = #{exclude_authrole => [two_role, three_role]},
        ?assertEqual(Subs,
                     ctrb_blackwhite_pubex:filter_subscriber(Subs0, Options, Id))
    after
        ok = stop_mock(Mock)
    end,
    ok.

blackwhite_eligible_test() ->
    Id = 1234,
    Subs0 = [123, 234, 345, 456, 567],
    Subs = [234, 456],
    Options = #{eligible => [234, 456]},
    ?assertEqual(Subs,
                 ctrb_blackwhite_pubex:filter_subscriber(Subs0, Options, Id)),
    ok.

blackwhite_eligible_id_test() ->
    {ok, Mock} = start_mock(),
    try
        Id = 1234,
        Subs0 = [123, 234, 345, 456, 567],
        Subs = [234, 456],
        Options = #{eligible_authid => [two_id, four_id]},
        ?assertEqual(Subs,
                     ctrb_blackwhite_pubex:filter_subscriber(Subs0, Options, Id))
    after
        ok = stop_mock(Mock)
    end,
    ok.

blackwhite_eligible_role_test() ->
    {ok, Mock} = start_mock(),
    try
        Id = 1234,
        Subs0 = [123, 234, 345, 456, 567],
        Subs = [123, 345],
        Options = #{eligible_authrole => [three_role, one_role]},
        ?assertEqual(Subs,
                     ctrb_blackwhite_pubex:filter_subscriber(Subs0, Options, Id))
    after
        ok = stop_mock(Mock)
    end,
    ok.

pubex_test() ->
    Id = 234,
    Subs0 = [123, 234, 345, 456, 567],
    Subs = [123, 345, 456, 567],
    ?assertEqual(Subs,
                 ctrb_blackwhite_pubex:filter_subscriber(Subs0, #{}, Id)),
    ?assertEqual(Subs,
                 ctrb_blackwhite_pubex:filter_subscriber(Subs0,
                                                         #{exclude_me => true},
                                                         Id)),
    ?assertEqual(Subs0,
                 ctrb_blackwhite_pubex:filter_subscriber(Subs0,
                                                         #{exclude_me => false},
                                                         Id)),
    ok.


start_mock() ->
    Modules = [cta_session],
    test_utils:meck_new(Modules),
    Lookup = fun(Id) ->
                     Session =
                         case Id of
                             123 -> one;
                             234 -> two;
                             345 -> three;
                             456 -> four;
                             567 -> five;
                             678 -> six;
                             789 -> seven;
                             890 -> eight;
                             901 -> nine;
                             _ -> anonymous
                         end,
                     {ok, Session}
             end,
    AuthId = fun(Session) ->
                     Id = <<"_id">>,
                     BinSession = atom_to_binary(Session, utf8),
                     binary_to_atom(<< BinSession/binary, Id/binary >>, utf8)
             end,
    AuthRole = fun(Session) ->
                       Role = <<"_role">>,
                       BinSession = atom_to_binary(Session, utf8),
                       binary_to_atom(<< BinSession/binary, Role/binary >>, utf8)
               end,
    ok = meck:expect(cta_session, lookup, Lookup),
    ok = meck:expect(cta_session, get_authid, AuthId),
    ok = meck:expect(cta_session, get_authrole, AuthRole),
    {ok, Modules}.



stop_mock(Modules) ->
    test_utils:meck_done(Modules),
    ok.
