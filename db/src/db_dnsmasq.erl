-module(db_dnsmasq).
-export([set_post_login_mac_address/1,
         clear_mac_addresses/1,
         clear_all_mac_addresses/0]).

-include_lib("apptools/include/log.hrl").

%%
%% set_post_login_mac_address
%%

set_post_login_mac_address(MacAddress) ->
    Parameters = [" --post ", MacAddress],
    dnsmasq_tool(Parameters).

%%
%% clear_mac_addresses
%%

clear_mac_addresses(MacAddresses) ->
    Parameters =
        lists:foldr(fun(MacAddress, Acc) ->
                            [" --clear ", MacAddress|Acc]
                    end, "", MacAddresses),
    dnsmasq_tool(Parameters).

%%
%% Exported: clear_all_mac_addresses
%%

clear_all_mac_addresses() ->
    dnsmasq_tool(" --clear-all").

%%
%% Utilities
%%

dnsmasq_tool(Parameters) ->
    ScriptPath = filename:join([code:priv_dir(main), "bin", "dnsmasq-tool"]),
    Command = [ScriptPath, " /etc/dnsmasq.conf", " --in-place --restart-dnsmasq",
               Parameters, "; echo $?"],
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
