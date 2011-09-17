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

-module(emangosd_protocol).

-author('Hanfei Shen <qqshfox@gmail.com>').

-export([behaviour_info/1]).

-export([setopts/2, send/2, close/1]).

behaviour_info(callbacks) ->
	[{init, 0},
		{on_connected, 1},
		{on_packet_received, 2},
		{on_disconnected, 1},
		{on_disconnected, 2}];
behaviour_info(_Other) ->
	undefined.

setopts(Socket, Options) ->
	inet:setopts(Socket, Options).

send(Socket, Packet) ->
	gen_tcp:send(Socket, Packet).

close(Socket) ->
	gen_tcp:close(Socket),
	self() ! {close, Socket}.
