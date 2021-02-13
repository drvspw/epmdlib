-module(epmdlib_common_dist).

-export([
         listen/1,
         select/1,
         accept/1,
         accept_connection/5,
         setup/5,
         close/1
        ]).


%%=========================================================================
%% API
%%=========================================================================
listen(Name) ->
  %% Here we figure out what port we want to listen on.
  Port = epmdlib:dist_port(Name),

  %% Set both "min" and "max" variables, to force the port number to
  %% this one.
  ok = application:set_env(kernel, inet_dist_listen_min, Port),
  ok = application:set_env(kernel, inet_dist_listen_max, Port),

  %% Finally execute the real function!
  Module = epmdlib:dist_proto_module(),
  Module:listen(Name).

select(Node) ->
  Module = epmdlib:dist_proto_module(),
  Module:select(Node).

accept(Listen) ->
  Module = epmdlib:dist_proto_module(),
  Module:accept(Listen).

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
  Module = epmdlib:dist_proto_module(),
  Module:accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime).

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
  Module = epmdlib:dist_proto_module(),
  Module:setup(Node, Type, MyNode, LongOrShortNames, SetupTime).

close(Listen) ->
  Module = epmdlib:dist_proto_module(),
  Module:close(Listen).
