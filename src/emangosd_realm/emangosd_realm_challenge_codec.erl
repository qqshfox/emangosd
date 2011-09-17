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

-module(emangosd_realm_challenge_codec).

-author('Hanfei Shen <qqshfox@gmail.com>').

-export([decode/1, encode/1]).

-include("realm_records.hrl").

decode(<<_Error, Size:16/little, Binary:Size/binary, Rest/binary>>) ->
	<<_Gamename:4/binary,
	_VersionMajor/little, _VersionMinor/little, _VersionBugfix/little,
	_Build:16/little, _Platform:4/binary, _OS:4/binary,
	_Country:4/binary, _TimezoneBias:32/little, _IP:4/binary,
	ILen, I:ILen/binary>> = Binary,
	{ok, #challenge{i=binary_to_list(I)}, Rest};
decode(_) ->
	{error, partial}.

encode(Record) ->
	<<>>.
