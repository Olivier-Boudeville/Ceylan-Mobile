% Copyright (C) 2019-2019 Olivier Boudeville
%
% This file is part of the Ceylan-Mobile library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Module offering the Ceylan-Mobile services.
%
% Operates through a Seaplus-based interface to the Gammu backend library.
%
-module(mobile).


% As is specifically overridden:
%-export([ get_backend_information/0 ]).


% For the Seaplus support:
-include("seaplus.hrl").



% API declaration.
%
% Note that the functions below have a spec, yet are not even defined here.


-type backend_type() :: 'gammu'.

-type backend_version() :: text_utils:ustring().


% Returns the name and version of the backend used.
-spec get_backend_information() -> { backend_type(), backend_version() }.



% We override this function for convenience: the C-side just returns the Gammu
% version as a string (ex: "1.40.0") and we use Myriad to easily convert it into
% {1,40,0}:
%
%% get_backend_information() ->

%%	{ Backend, VersionString } = seaplus:call_port_for(
%%					   seaplus:get_service_port_key_for( ?MODULE ), 1, [] ),

%%	VersionTuple = basic_utils:parse_version( VersionString ),

%%	{ Backend, VersionTuple }.
