% Copyright (C) 2018-2019 Olivier Boudeville
%
% This file is part of the Ceylan-Seaplus library.
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
% Creation date: Sunday, March 24, 2019


% Allows to test the Ceylan-Mobile services.
%
-module(mobile_test).


-export([ run/0 ]).



run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the Ceylan-Mobile service." ),

	% Not mobile:start_link(), as here we want to survive a crash of the mobile
	% service (i.e. to be able to handle failures explicitly, as messages
	% received by this test process):
	%
	mobile:start(),

	Info = mobile:get_backend_information(),

	test_facilities:display( "Back-end information: ~p.", [ Info ] ),

	test_facilities:display( "Device manufacturer: ~s.",
							 [ mobile:get_device_manufacturer() ] ),

	test_facilities:display( "Device model: ~s.",
							 [ mobile:get_device_model() ] ),


	{ RevisionText, DateText, RevisionNumber } =
		mobile:get_firmware_information(),

	test_facilities:display( "Firmware information: revision is '~s', "
							 "date is '~s' and revision number is ~w.",
							 [ RevisionText, DateText, RevisionNumber ] ),


	test_facilities:display( "IMEI code: '~s'.", [ mobile:get_imei_code() ] ),

	test_facilities:display( "Hardware information: '~s'.",
							 [ mobile:get_hardware_information() ] ),

	test_facilities:display( "IMSI code: '~s'.", [ mobile:get_imsi_code() ] ),

	{ SignalStrength, SignalStrengthPercent, ErrorRate } =
		mobile:get_signal_quality(),

	test_facilities:display( "Signal quality: signal strength is ~B dBm (~B%), "
			 "error rate is ~B%.",
			 [ SignalStrength, SignalStrengthPercent, ErrorRate ] ),

	mobile:stop(),

	test_facilities:stop().
