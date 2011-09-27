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

-module(emangosd_sup).

-author('Hanfei Shen <qqshfox@gmail.com>').

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD3(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	start_link([]).

start_link(Servers) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Servers).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Servers) ->
	ConnectionSup = ?CHILD(emangosd_connection_sup, supervisor),
	AcceptorSup = ?CHILD3(emangosd_acceptor_sup, supervisor, Servers),
	Children = [ConnectionSup, AcceptorSup],
	RestartStrategy = {one_for_one, 5, 10},
	{ok, {RestartStrategy, Children}}.
