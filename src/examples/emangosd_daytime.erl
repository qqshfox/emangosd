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

-module(emangosd_daytime).

-author('Hanfei Shen <qqshfox@gmail.com>').

-behaviour(emangosd_protocol).

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
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init() ->
	{ok, []}.

on_connected(Socket) ->
	error_logger:info_report([on_connected, {socket, Socket}]),
	DaytimeString = get_daytime(),
	send(Socket, DaytimeString),
	close(Socket),
	{ok, []}.

on_packet_received(Socket, Packet, State) ->
	error_logger:info_report([on_packet_received, {socket, Socket}, {packet, Packet}, {state, State}]),
	{ok, State}.

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

get_daytime() ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time_to_local_time(erlang:universaltime()),
	DayOfWeek = calendar:day_of_the_week({Year, Month, Day}),
	DayName = httpd_util:day(DayOfWeek),
	MonthName = httpd_util:month(Month),
	DayString = io_lib:format("~s, ~s ~p, ~p ~p:~p:~p~n",
		[DayName, MonthName,
			Day, Year,
			Hour, Minute, Second]),
	lists:flatten(DayString).
