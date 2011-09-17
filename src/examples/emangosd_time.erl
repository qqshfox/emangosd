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

-module(emangosd_time).

-author('Hanfei Shen <qqshfox@gmail.com>').

-behaviour(emangosd_protocol).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% emangosd_protocol Function Exports
%% ------------------------------------------------------------------

-export([init/0, on_connected/1, on_packet_received/2,
		on_disconnected/1, on_disconnected/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% emangosd_protocol Function Definitions
%% ------------------------------------------------------------------

init() ->
	{ok, []}.

on_connected(Socket) ->
	error_logger:info_report([on_connected, {socket, Socket}]),
	Time = get_time(),
	send(Socket, Time),
	close(Socket),
	ok.

on_packet_received(Socket, Packet) ->
	error_logger:info_report([on_packet_received, {socket, Socket}, {packet, Packet}]),
	ok.

on_disconnected(Socket) ->
	on_disconnected(Socket, ok).

on_disconnected(Socket, Reason) ->
	error_logger:info_report([on_disconnected, {socket, Socket}, {reason, Reason}]),
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send(Socket, Packet) ->
	error_logger:info_report([send, {socket, Socket}, {packet, Packet}]),
	emangosd_protocol:send(Socket, Packet).

close(Socket) ->
	error_logger:info_report([close, {socket, Socket}]),
	emangosd_protocol:close(Socket).

get_time() ->
	UnixTimestamp = emangosd_utils:get_unix_timestamp(),
	error_logger:info_report(UnixTimestamp),
	<<UnixTimestamp:32>>.
