%%%------------------------------------------------------------------
%%% emangosd
%%% Copyright (C) 2011 Hanfei Shen <qqshfox@gmail.com>
%%%
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%%%------------------------------------------------------------------

-module(emangosd_realm).

-author('Hanfei Shen <qqshfox@gmail.com>').

-behaviour(emangosd_protocol).

-include("realm_records.hrl").

-define(OPTS, [{active, once}]).

-record(state, {rest=(<<>>),
		logon_state=#logon_state{}}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% emangosd_protocol Function Exports
%% ------------------------------------------------------------------

-export([init/0, on_connected/1, on_packet_received/3,
		on_disconnected/1, on_disconnected/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% emangosd_protocol Function Definitions
%% ------------------------------------------------------------------

init() ->
	{ok, ?OPTS}.

on_connected(Socket) ->
	error_logger:info_report([on_connected, {socket, Socket}]),
	{ok, #state{}}.

on_packet_received(Socket, Packet, #state{rest=Rest}=State) ->
	error_logger:info_report([on_packet_received, {socket, Socket}, {packet, Packet}, {state, State}]),
	PartialPacket = <<Rest/binary, Packet/binary>>,
	{ok, NewState} = on_partial_packet_received(Socket, PartialPacket, State#state{rest=(<<>>)}),
	setopts(Socket),
	{ok, NewState}.

on_disconnected(Socket) ->
	on_disconnected(Socket, ok).

on_disconnected(Socket, Reason) ->
	error_logger:info_report([on_disconnected, {socket, Socket}, {reason, Reason}]),
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

on_partial_packet_received(Socket, Packet, #state{logon_state=LogonState}=State) ->
	error_logger:info_report([on_partial_packet_received, {socket, Socket}, {packet, Packet}, {state, State}]),
	<<Opcode, Data/binary>> = Packet,
	Handler = emangosd_realm_opcodes:get_handler(Opcode),
	error_logger:info_report([{handler, Handler}, {data, Data}, {logon_state, LogonState}]),
	{Action, NewRest, NewLogonState} = emangosd_realm_handler:Handler(Data, LogonState),
	error_logger:info_report([{action, Action}, {new_rest, NewRest}, {new_logon_state, NewLogonState}]),
	case Action of
		ok -> ok;
		{send, PacketToSend} ->
			error_logger:info_report([Action]),
			send(Socket, [Opcode, PacketToSend])
	end,
	error_logger:info_report([{handler, Handler}, {new_rest, NewRest}, {new_logon_state, NewLogonState}]),
	{ok, State#state{rest=NewRest, logon_state=NewLogonState}}.

setopts(Socket) ->
	emangosd_protocol:setopts(Socket, ?OPTS).

recv(Socket, Length) ->
	emangosd_protocol:recv(Socket, Length).

recv(Socket, Length, Timeout) ->
	emangosd_protocol:recv(Socket, Length, Timeout).

send(Socket, Packet) ->
	error_logger:info_report([send, {socket, Socket}, {packet, Packet}]),
	emangosd_protocol:send(Socket, Packet).

close(Socket) ->
	error_logger:info_report([close, {socket, Socket}]),
	emangosd_protocol:close(Socket).
