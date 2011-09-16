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

-module(emangosd_app).

-author('Hanfei Shen <qqshfox@gmail.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	webtool:start(standard_path, [{port, 8888}, {bind_address, {192, 168, 0, 29}}, {server_name, "hanfei-gentoo"}]),
	case emangosd_sup:start_link() of
		{ok, _Pid} = Ok ->
			Callback = emangosd_daytime,
			Port = 1863,
			Acceptors = 1,
			emangosd_listener:listen(Callback, Port, Acceptors),
			Ok;
		Other ->
			Other
	end.

stop(_State) ->
	ok.