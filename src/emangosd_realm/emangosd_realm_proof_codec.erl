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

-module(emangosd_realm_proof_codec).

-author('Hanfei Shen <qqshfox@gmail.com>').

-export([decode/1, encode/1]).

-include("realm_records.hrl").

decode(<<A:256/little, M1:160/little, _CRC:160/little, _NumberOfKeys, _SecurityFlags, Rest/binary>>) ->
	{ok, #proof{a=A, m1=M1}, Rest};
decode(_) ->
	{error, partial}.

encode(Record) ->
	<<>>.
