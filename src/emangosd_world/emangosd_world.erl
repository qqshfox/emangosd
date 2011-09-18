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

-module(emangosd_world).

-author('Hanfei Shen <qqshfox@gmail.com>').

-behaviour(emangosd_protocol).

-include("world_records.hrl").

-define(OPTS, [{active, once}]).

-record(state, {rest=(<<>>)}).

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

on_partial_packet_received(Socket, Packet, State) ->
	error_logger:info_report([on_partial_packet_received, {socket, Socket}, {packet, Packet}, {state, State}]),
	<<Opcode, Data/binary>> = Packet,
	Handler = emangosd_world_opcodes:get_handler(Opcode),
	error_logger:info_report([{handler, Handler}, {data, Data}]),
	{Action, NewRest} = emangosd_world_handler:Handler(Data),
	error_logger:info_report([{action, Action}, {new_rest, NewRest}]),
	case Action of
		ok -> ok;
		{send, PacketToSend} ->
			error_logger:info_report([Action]),
			send(Socket, [Opcode, PacketToSend])
	end,
	error_logger:info_report([{handler, Handler}, {new_rest, NewRest}]),
	{ok, State#state{rest=NewRest}}.

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
