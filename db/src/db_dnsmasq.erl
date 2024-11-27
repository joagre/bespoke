-module(db_dnsmasq).
-export([set_post_login_mac_address/1,
         clear_mac_addresses/1,
         clear_all_mac_addresses/0]).
-export_type([mac_address/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").

-type mac_address() :: binary().

%%
%% set_post_login_mac_address
%%

-spec set_post_login_mac_address(mac_address()) -> ok | {error, string()}.

set_post_login_mac_address(MacAddress) ->
    Parameters = " --post " ++ ?b2l(MacAddress),
    dnsmasq_tool(Parameters).

%%
%% clear_mac_addresses
%%

-spec clear_mac_addresses([mac_address()]) -> ok | {error, string()}.

clear_mac_addresses(MacAddresses) ->
    Parameters =
        lists:foldr(fun(MacAddress, Acc) ->
                            [" --clear ", ?b2l(MacAddress)|Acc]
                    end, [], MacAddresses),
    dnsmasq_tool(Parameters).

%%
%% Exported: clear_all_mac_addresses
%%

-spec clear_all_mac_addresses() -> ok | {error, string()}.

clear_all_mac_addresses() ->
    dnsmasq_tool(" --clear-all").

%%
%% Utilities
%%

dnsmasq_tool(Parameters) ->
    ScriptPath =
        filename:absname(
          filename:join([code:lib_dir(main), "bin", "dnsmasq-tool"])),
    Command = ["sudo ", ScriptPath, " /etc/dnsmasq.conf",
               " --restart-dnsmasq", Parameters, " 2>&1; echo $?"],
    io:format("Calling: ~s\n", [Command]),
    ok.
%    case string:strip(os:cmd(Command)) of
%        "0" ->
%            ok;
%        UnexpectedOutput ->
%            ?log_error("Unexpected output from dnsmasq-tool: ~s",
%                       [UnexpectedOutput]),
%            {error, UnexpectedOutput}
%    end.
