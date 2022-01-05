% Copyright (C) 2019-2022 Olivier Boudeville
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


% @doc Module for the test of the Ceylan-Mobile services regarding <b>SMS
% classes</b>.
%
% See also: http://www.ozekisms.com/index.php?owpn=544
%
-module(mobile_class_test).


-export([ run/0 ]).


% We use Myriad's 'preferences' module in order to allow the user to define
% their own settings and not expose them (ex: mobile phone number to use here).
%
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the Ceylan-Mobile services "
							 "regarding SMS classes." ),

	mobile_test:set_up_mobile_environment(),

	% Not mobile:start_link(), as here we want to survive a crash of the mobile
	% service (i.e. to be able to handle failures explicitly, as messages
	% received by this test process):
	%
	mobile:start(),

	case preferences:get( mobile_number ) of

		undefined ->
			test_facilities:display( "No registered preference regarding a "
				"target mobile number, no actual sending performed." );

		MobileNumber ->
			actual_sending_test( MobileNumber )

	end,

	mobile:stop(),

	test_facilities:stop().



% Performs actual test sendings.
actual_sending_test( MobileNumber ) ->

	% The tested SMS classes:
	Classes = [ 0, 1, 2, 3, 4 ],

	MessageFormat = "Hello class #~B, âêîôû!",

	test_facilities:display( "~n~nThe next sending-related  tests will target "
		"the following recipient mobile number: '~ts', with SMS of following "
		"classes: ~w.", [ MobileNumber, Classes ] ),

	Reports = [ mobile:send_sms( text_utils:format( MessageFormat, [ Cl ] ),
								 MobileNumber, Cl ) || Cl <- Classes ],

	test_facilities:display( "~nReports: ~w.", [ Reports ] ).
