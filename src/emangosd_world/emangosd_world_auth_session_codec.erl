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

-module(emangosd_world_auth_session_codec).

-author('Hanfei Shen <qqshfox@gmail.com>').

-export([decode/1, encode/1]).

-include("world_records.hrl").

decode(<<Build:32/little, _:32, Rest/binary>>) ->
	{ok, Account, ClientSeed, Digest} = decode_more(Rest),
	{ok, Build, Account, ClientSeed, Digest}.

encode(Record) ->
	<<>>.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

decode_more(Binary) ->
	decode_more(Binary, "").

decode_more(<<0, _:32, ClientSeed:4/binary, _:32, _:32, _:32, _:64, Digest:20/binary, _Rest/binary>>, Account) ->
	{ok, lists:reverse(Account), ClientSeed, Digest};
decode_more(<<C, Rest/binary>>, Account) ->
	decode_more(Rest, [C | Account]).
