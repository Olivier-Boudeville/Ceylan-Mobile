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

	test_facilities:display( "Device name: ~s.",
							 [ mobile:get_device_name() ] ),

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

	case preferences:get( mobile_number ) of

		undefined ->
			test_facilities:display( "No registered preference regarding a "
									 "target mobile number, no actual sending "
									 "performed." );

		MobileNumber ->
			actual_sending_test( MobileNumber )

	end,

	mobile:stop(),

	test_facilities:stop().



% An actual test done, should the sending of test SMS be enabled.
actual_sending_test( MobileNumber ) ->

	test_facilities:display( "Sending tests will target the following "
							 "recipient mobile number: '~s'.",
							 [ MobileNumber ] ),

	FirstSMSReport = mobile:send_sms( "Hello world!", MobileNumber ),

	test_facilities:display( "Sent first SMS whose report is: ~p.",
							 [ FirstSMSReport ] ),

	SecondSMSReport = mobile:send_sms(
		"Goodbye! This is a longer SMS to test their support."
		"For thee the wonder-working earth puts forth sweet flowers. "
		"-- Titus Lucretius Carus"
		"Ho! Tom Bombadil, Tom Bombadillo! "
		"By water, wood and hill, by reed and willow, "
		"By fire, sun and moon, harken now and hear us!"
		"Come, Tom Bombadil, for our need is near us! "
		"-- J. R. R. Tolkien", MobileNumber ),

	test_facilities:display( "Sent second SMS whose identifier is: ~p.",
							 [ SecondSMSReport ] ).
