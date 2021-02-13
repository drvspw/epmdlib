-module(epmdlib).

-export([
         dist_port/1,
         dist_proto_module/0
        ]).


-include("epmdlib.hrl").


%%=========================================================================
%% API
%%=========================================================================
%% Return the port number to be used by a certain node.
dist_port(Name) when is_atom(Name) ->
  dist_port(atom_to_list(Name));

dist_port(Name) when is_list(Name) ->
  %% Figure out the base port.  If not specified using the
  %% inet_dist_base_port kernel environment variable, set to default port.
  BasePort = application:get_env(kernel, inet_dist_base_port, ?EPMDLIB_BASE_PORT),

  %% Now, figure out our "offset" on top of the base port.
  Offset = compute_offset(Name),

  %% actual port is BasePort + Offset
  BasePort + Offset.

%% return the actual protocol module configured in vm.args
dist_proto_module() ->
  case init:get_argument(?EPMDLIB_DIST_TRANSPORT) of
    {ok, [[ProtoModule]]} ->
      dist_proto_module(ProtoModule);
    _ ->
      dist_proto_module("tcp")
  end.

%%=========================================================================
%% private functions
%%=========================================================================
dist_proto_module("tcp") ->
  inet_tcp_dist;

dist_proto_module("tcp6") ->
  inet6_tcp_dist;

dist_proto_module("tls") ->
  inet_tls_dist;

dist_proto_module("tls6") ->
  inet6_tls_dist;

dist_proto_module(_) ->
  inet_tcp_dist.

compute_offset(Name) ->
  %% The offset is the integer just to the left of the @ sign in our node
  %% name.  If there is no such number, the offset is 0.
  %%
  %% Also handle the case when no hostname was specified.
  NodeName = re:replace(Name, "@.*$", ""),
  case re:run(NodeName, "[0-9]+$", [{capture, first, list}]) of
    nomatch ->
      0;
    {match, [OffsetAsString]} ->
      list_to_integer(OffsetAsString)
  end.

%%=========================================================================
%% Unit Test Suite
%%=========================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

dist_port_suite_test_() ->
  [
   ?_assertMatch(?EPMDLIB_BASE_PORT, epmdlib:dist_port('sname@localhost')),
   ?_assertMatch(?EPMDLIB_BASE_PORT+1, epmdlib:dist_port('sname1@localhost')),
   ?_assertMatch(?EPMDLIB_BASE_PORT+2, epmdlib:dist_port('name2@host.domain.com')),
   ?_assertMatch(?EPMDLIB_BASE_PORT+3, epmdlib:dist_port('name3@host-01.domain.net'))
  ].

-endif.
