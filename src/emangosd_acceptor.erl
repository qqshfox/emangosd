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

-module(emangosd_acceptor).

-author('Hanfei Shen <qqshfox@gmail.com>').

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {
		lsocket,
		callback}).

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

start_link(Callback, LSocket) ->
	gen_server:start_link(?MODULE, [Callback, LSocket], [{debug, [trace]}]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Callback, LSocket]) ->
	{ok, #state{lsocket=LSocket, callback=Callback}, 0}.

handle_call(_Request, _From, State) ->
	{noreply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, #state{lsocket=LSocket, callback=Callback}=State) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			{ok, Pid} = emangosd_connection_sup:start_child(Callback, Socket),
			case gen_tcp:controlling_process(Socket, Pid) of
				ok    -> gen_server:cast(Pid, {connected, Socket});
				Other -> exit(Pid, Other)
			end,
			{noreply, State, 0};
		Error ->
			error_logger:error_report(Error),
			{stop, normal, State}
	end;
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

