-module(ctr_subscription).

-include("ct_router.hrl").

-export([to_map/1]).


to_map(#ctr_subscription{id = Id, created = Created, uri = Uri,
                         match = Match }) ->
    #{id => Id, created => iso8601:format(Created), match => Match, uri => Uri}.
