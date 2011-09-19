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
-include("world_opcodes.hrl").

-define(OPTS, [{active, once}]).

-record(state, {
		rest=(<<>>),
		crypto_state=#crypto_state{}}).

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
	State = #state{},
	error_logger:info_report([on_connected, {socket, Socket}]),
	send_packet(Socket, ?SMSG_AUTH_CHALLENGE, <<1:32/little, 0:32, 0:128>>),
	{ok, #state{}}.

on_packet_received(Socket, Packet, #state{rest=Rest}=State) ->
	error_logger:info_report([on_packet_received, {socket, Socket}, {packet, Packet}, {state, State}]),
	PartialPacket = <<Rest/binary, Packet/binary>>,
	{ok, NewState} = on_header_received(Socket, PartialPacket, State#state{rest=(<<>>)}),
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

on_header_received(Socket, <<Header:6/binary, Rest/binary>>, #state{crypto_state=CryptoState}=State) ->
	error_logger:info_report([on_header_received, {socket, Socket}, {header, Header}, {rest, Rest}, {state, State}]),
	DecryptedHeader = emangosd_world_crypto:decrypt(Header, CryptoState),
	on_body_received(Socket, <<DecryptedHeader/binary, Rest/binary>>, State);
on_header_received(_Socket, Binary, #state{rest=Rest}=State) ->
	{ok, State#state{rest=(<<Rest/binary, Binary/binary>>)}}.

on_body_received(Socket, <<PacketSize:16, Packet:PacketSize/binary, Rest/binary>>, #state{rest=OldRest}=State) ->
	error_logger:info_report([on_body_received, {socket, Socket}, {packet_size, PacketSize}, {packet, Packet}, {rest, Rest}, {state, State}]),
	<<Opcode:32/little, Body/binary>> = Packet,
	Handler = emangosd_world_opcodes:get_handler(Opcode),
	error_logger:info_report([{handler, Handler}, {body, Body}]),
	case emangosd_world_handler:Handler(Body, []) of
		ok -> ok;
		{send, PacketToSend} ->
			send(Socket, PacketToSend)
	end,
	NewRest = <<OldRest/binary, Rest/binary>>,
	error_logger:info_report([{handler, Handler}, {new_rest, NewRest}]),
	{ok, State#state{rest=NewRest}};
on_body_received(_Socket, Binary, #state{rest=Rest}=State) ->
	{ok, State#state{rest=(<<Rest/binary, Binary/binary>>)}}.

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

send_packet(Socket, Opcode, Packet) ->
	error_logger:info_report([send_packet, {socket, Socket}, {opcode, Opcode}, {packet, Packet}, {packet_size, size(Packet)}]),
	Data = <<Opcode:16/little, Packet/binary>>,
	send_packet(Socket, Data).

send_packet(Socket, Data) ->
	Size = size(Data),
	DataToSend = <<Size:16, Data/binary>>,
	send(Socket, DataToSend).
