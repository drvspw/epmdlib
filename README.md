## epmdlib

This is a embedded epmd library based on [Erlang (and Elixir) distribution without epmd](https://www.erlang-solutions.com/blog/erlang-and-elixir-distribution-without-epmd.html)

## Usage
1. Add *epmdlib* as a dependency in *rebar.confg*
```
{deps, [
        {epmdlib, "1.0.0"}
    ]}.
```

2. Add the following lines to *vm.args* file:
```
-proto_dist epmdlib_common
-start_epmd false
-epmd_module epmdlib_epmd_client
-epmdlib_dist_transport tcp
```

For Ipv6
```
-epmdlib_dist_transport tcp6
```

If `-epmdlib_dist_transport` argument is not specified, then `epmdlib` defaults to using `tcp` for the distribution protocol's transport layer.


To setup distribution over TLS, add `-ssl_dist_optfile "/path/to/dist.conf"` to *vm.args* file. Also change `-epmdlib_dist_transport` option to `tls` or `tls6`. Then add the following information to `/path/to/dist.conf` file
```
[{server,
  [{certfile, "/path/to/certfile"},
   {keyfile, "/path/to/keyfile"},
   {cacertfile, "/path/to/cacertfile"},
   {verify, verify_peer},
   {fail_if_no_peer_cert, true},
   {secure_renegotiate, false}]},

 {client,
   [{certfile, "/path/to/certfile"},
   {keyfile, "/path/to/keyfile"},
   {cacertfile, "/path/to/cacertfile"},
   {verify, verify_peer},
   {secure_renegotiate, false}]}].
```

# NOTE
- When deploying an OTP application that uses *epmdlib* (for the first time), please make sure to kill any existing *epmd* process started by the previours version of the OTP application.

- By default the library uses 4469 as the base port. If that port is not availabe, configure a different base port in your application's `sys.config`.
```erlang
[
{kernel, [
	{inet_dist_base_port, PORT} %% an available port
]}
].
```

## Testing
Start two nodes `foo1`, `foo2` and have them connect to each other.

```bash
erl -pa _build/default/lib/epmdlib/ebin/ -proto_dist epmdlib_common -start_epmd false -epmd_module epmdlib_epmd_client -epmdlib_dist_protocol tcp -sname foo1@localhost
```

```bash
erl -pa _build/default/lib/epmdlib/ebin/ -proto_dist epmdlib_common -start_epmd false -epmd_module epmdlib_epmd_client -epmdlib_dist_protocol tcp -sname foo2@localhost
```

If it is working correctly,
```
Erlang/OTP 22 [erts-10.6.2] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [hipe]

Eshell V10.6.2  (abort with ^G)
(foo1@localhost)1> net_adm:ping('foo2@localhost').
pong
(foo1@localhost)2>
```

## Contributing
1. Update the version in `src/epmdlib.app.src`
2. Make changes and test
3. Update *Usage* section in `README.md` to reflect the latest version of the library
4. Commit and push the changes
5. Publish the package
