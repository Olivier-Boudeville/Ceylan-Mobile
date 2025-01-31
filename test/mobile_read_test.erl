% Copyright (C) 2019-2025 Olivier Boudeville
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
% Creation date: Sunday, March 24, 2019.

-module(mobile_read_test).

-moduledoc """
Module for the test of the Ceylan-Mobile services regarding the **reading of
SMS**.

Note that with the 'dummy' Gammu model, TPMR references might be always 255.
""".


-export([ run/0 ]).



run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display(
		"Testing the Ceylan-Mobile service regarding the reading of SMS." ),

	mobile_test:set_up_mobile_environment(),

	% Not mobile:start_link(), as here we want to survive a crash of the mobile
	% service (i.e. to be able to handle failures explicitly, as messages
	% received by this test process):
	%
	mobile:start(),

	% By default, do not delete read SMS:
	DeleteOnReading = false,
	%DeleteOnReading = true,

	case mobile:read_all_sms( DeleteOnReading ) of

		[] ->
			test_facilities:display( "No SMS to read found." );

		SMSList ->
			test_facilities:display( "~B SMS read: ~ts",
				[ length( SMSList ), text_utils:strings_to_enumerated_string(
				  [ mobile:received_sms_to_string( S ) || S <- SMSList ] ) ] )

	end,

	mobile:stop(),

	test_facilities:stop().
