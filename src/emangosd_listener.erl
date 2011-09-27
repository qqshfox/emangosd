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

-module(emangosd_listener).

-author('Hanfei Shen <qqshfox@gmail.com>').

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {
    lsocket}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_link/1, listen/1, listen/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	start_link([]).

start_link(Servers) ->
    {ok, _State} = Ok = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
	[listen(Server) || Server <- Servers],
	Ok.

listen({Callback, Port, Acceptors}) ->
	listen(Callback, Port, Acceptors).
	
listen(Callback, Port, Acceptors) ->
	gen_server:call(?SERVER, {listen, Callback, Port, Acceptors}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	{ok, #state{}}.

handle_call({listen, Callback, Port, Acceptors}, _From, State) ->
	{ok, LSocket} = gen_tcp:listen(Port, [binary,
			{reuseaddr, true}]),
	start_acceptors(Callback, LSocket, Acceptors),
	{reply, ok, State#state{lsocket=LSocket}};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_acceptors(Callback, LSocket, Acceptors) when Acceptors > 0 ->
	emangosd_acceptor_pool_sup:start_child(Callback, LSocket),
	start_acceptors(Callback, LSocket, Acceptors - 1);
start_acceptors(_Callback, _LSoscket, 0) ->
	ok.
