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

-module(emangosd_connection).

-author('Hanfei Shen <qqshfox@gmail.com>').

-behaviour(gen_server).

-record(state, {
		socket,
		callback,
		cb_state}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Callback, Socket) ->
	gen_server:start_link(?MODULE, [Callback, Socket], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Callback, Socket]) ->
	{ok, #state{socket=Socket, callback=Callback}, 0}.

handle_call(_Request, _From, State) ->
	{noreply, ok, State}.

handle_cast({connected, Socket}, #state{callback=Callback}=State) ->
	{ok, Options} = Callback:init(),
	emangosd_protocol:setopts(Socket, Options),
	{ok, CBState} = Callback:on_connected(Socket),
	{noreply, State#state{cb_state=CBState}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, Socket, Packet}, #state{callback=Callback, cb_state=CBState}=State) ->
	{ok, NewCBState} = Callback:on_packet_received(Socket, Packet, CBState),
	{noreply, State#state{cb_state=NewCBState}};
handle_info({tcp_closed, Socket}, #state{callback=Callback}=State) ->
	Callback:on_disconnected(Socket),
	{stop, normal, State};
handle_info({tcp_error, Socket, Reason}, #state{callback=Callback}=State) ->
	Callback:on_disconnected(Socket, Reason),
	{stop, normal, State};
handle_info({close, Socket}, #state{callback=Callback}=State) ->
	Callback:on_disconnected(Socket),
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

