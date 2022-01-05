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


% @doc Module for the overall, most complete test of the <b>Ceylan-Mobile
% services</b>.
%
% Some test sentences emanate from dear /usr/bin/fortune.
%
% Note that with the 'dummy' Gammu model, TPMR references might be always 255.
%
-module(mobile_test).


-export([ run/0, set_up_mobile_environment/0 ]).


% @doc Sets up a relevant environment for the execution of Mobile (and Seaplus).
%
% (shared between tests)
%
set_up_mobile_environment() ->

	test_facilities:display( "Setting up a Mobile-compliant environment." ),

	% Here the driver (i.e. mobile_seaplus_driver) has been generated in another
	% directory (in 'src'), so we have to ensure that Seaplus will be able to
	% find it at runtime:
	%
	system_utils:add_path_for_executable_lookup( "../src" ),

	% We also need to secure the Seaplus library itself
	% (i.e. libseaplus-x.y.z.so), thus to locate the Seaplus dependency itself;
	% we consider here that it can be found either as a sibling tree of this
	% Mobile one (if using our native conventions), or in a rebar3 build tree
	% (if using this tool). Hence:
	%
	system_utils:add_paths_for_library_lookup(
		[ "../../seaplus/src/", "../_build/default/lib/seaplus/src/" ] ).



run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the Ceylan-Mobile service." ),

	set_up_mobile_environment(),

	% Not mobile:start_link(), as here we want to survive a crash of the mobile
	% service (i.e. to be able to handle failures explicitly, as messages
	% received by this test process):
	%
	mobile:start(),

	case mobile:is_available() of

		true ->
			test_facilities:display( "Ceylan-Mobile reported as available, "
				"performing then an actual testing thereof." ),
			actual_test();

		false ->
			throw( mobile_not_available )

	end.


% Perfoms the actual testing.
actual_test() ->

	Info = mobile:get_backend_information(),

	test_facilities:display( "Back-end information: ~p.", [ Info ] ),

	test_facilities:display( "Has an actual device: ~ts.",
							 [ mobile:has_actual_device() ] ),

	test_facilities:display( "Device name: ~ts.",
							 [ mobile:get_device_name() ] ),

	test_facilities:display( "Device manufacturer: ~ts.",
							 [ mobile:get_device_manufacturer() ] ),

	test_facilities:display( "Device model: ~ts.",
							 [ mobile:get_device_model() ] ),

	{ RevisionText, DateText, RevisionNumber } =
		mobile:get_firmware_information(),

	test_facilities:display( "Firmware information: revision is '~ts', "
		"date is '~ts' and revision number is ~w.",
		[ RevisionText, DateText, RevisionNumber ] ),


	test_facilities:display( "IMEI code: '~ts'.", [ mobile:get_imei_code() ] ),


	% This operation may not be supported by the end device:
	try

		test_facilities:display( "Hardware information: '~ts'.",
								 [ mobile:get_hardware_information() ] )

	catch

		HardwareInfoException ->
			test_facilities:display( "Failed to get hardware information: ~p",
									 [ HardwareInfoException ] )

	end,

	test_facilities:display( "IMSI code: '~ts'.", [ mobile:get_imsi_code() ] ),

	{ SignalStrength, SignalStrengthPercent, ErrorRate } =
		mobile:get_signal_quality(),

	test_facilities:display( "Signal quality: signal strength is ~B dBm (~B%), "
		"error rate is ~B%.",
		[ SignalStrength, SignalStrengthPercent, ErrorRate ] ),

	test_facilities:display( "Overall, textual information about the "
		"current mobile setting: '~ts'.",
		[ mobile:get_textual_information() ] ),


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

	% Testing the most common SMS class:
	Class = 1,

	test_facilities:display( "~n~nThe next sending tests will target the "
		"following recipient mobile number: '~ts', first with a few "
		"single-part SMS, of various lengths, of class ~B, and needing "
		"various encodings.", [ MobileNumber, Class ] ),


	% Single (non-multipart) SMS testing:

	% Of course short enough not to be truncated:
	FirstMessage = "Hello world!",

	FirstSMSReport = mobile:send_regular_sms( FirstMessage, MobileNumber ),

	test_facilities:display( "~nSent first (single-part) SMS (message: '~ts') "
		"with default settings, whose report is: ~w.",
		[ FirstMessage, FirstSMSReport ] ),


	% With (uncompressed) GSM encoding, the single received SMS should stop just
	% after the second 'Tom':
	%
	SecondMessage = text_utils:format(
		"Goodbye! This is a longer SMS to test their support. "
		"See http://mobile.esperide.org for further details.~n"
		"For thee the wonder-working earth puts forth sweet flowers. "
		"-- Titus Lucretius Carus~n"
		"Ho! Tom Bombadil, Tom Bombadillo!~n"
		"By water, wood and hill, by reed and willow,~n"
		"By fire, sun and moon, harken now and hear us!~n"
		"Come, Tom Bombadil, for our need is near us!~n"
		"-- J. R. R. Tolkien", [] ),

	SecondSMSReport = mobile:send_regular_sms( SecondMessage, MobileNumber ),

	test_facilities:display( "~nSent second (single-part) SMS (message: '~ts') "
		"with default settings, whose report is: ~w.",
		[ SecondMessage, SecondSMSReport ] ),


	EncodingTestFormatMsg = "This is a text sent in ~ts: aéàùâêîôû; "
		"this is a longer message meant to be truncated should a single SMS "
		"be used (instead of a multi-part one, involving multiple actual SMS "
		"that are to be collected and reassembled by the end device.~n"
		"If it happens once, it's a bug. If it happens twice, it's a feature. "
		"If it happens more than twice, it's a design philosophy.~n"
		"Beauty, n.: The power by which a woman charms a lover and terrifies "
		"a husband. -- Ambrose Bierce",

	GSMUncompMsg = text_utils:format( EncodingTestFormatMsg,
									  [ "GSM uncompressed" ] ),

	%test_facilities:display( "Will be sending now following message: '~ts'.",
	%						 [ GSMUncompMsg ] ),

	% With this encoding, "âêîôû" is expected to be sent as "aeiou", and the
	% received SMS is expected to stop after 'one, inv':
	%
	GSMUncompSMSReport = mobile:send_regular_sms( GSMUncompMsg, MobileNumber,
												  Class, gsm_uncompressed ),

	test_facilities:display( "~nSent (single-part) SMS (message: '~ts') "
		"for the test of GSM uncompressed encoding, whose report is: ~w.",
		[ GSMUncompMsg, GSMUncompSMSReport ] ),



	UnicodeUncompMsg = text_utils:format( EncodingTestFormatMsg,
										 [ "Unicode uncompressed" ] ),

	%test_facilities:display( "Sending now: '~ts'.", [ UnicodeUncompMsg ] ),

	% With Unicode, "âêîôû" is expected to be received just fine, but the length
	% of this single SMS shall be quite small: stopping after "this is a long":
	%
	UnicodeUncompSMSReport = mobile:send_regular_sms( UnicodeUncompMsg,
								   MobileNumber, Class, unicode_uncompressed ),

	test_facilities:display( "~nSent (single-part) SMS (message: '~ts') "
		"for the test of Unicode uncompressed encoding, whose report is: ~w.",
		[ UnicodeUncompMsg, UnicodeUncompSMSReport ] ),


	AutoMsg = text_utils:format( EncodingTestFormatMsg,
								 [ "automatic encoding selection mode" ] ),

	%test_facilities:display( "Sending now: '~ts'.", [ AutoMsg ] ),

	AutoSMSReport = mobile:send_regular_sms( AutoMsg, MobileNumber ),

	test_facilities:display( "~nSent (single-part) SMS (message: '~ts') "
		"for the test of (automatic) encoding, whose report is: ~w.",
		[ AutoMsg, AutoSMSReport ] ),



	% Now, multipart testing:

	test_facilities:display( "~n~nNow sending the same kind of messages, "
							 "this time using multipart SMS." ),


	FirstMultiSMSReport = mobile:send_multipart_sms( FirstMessage,
													 MobileNumber ),

	test_facilities:display( "~nSent first multipart SMS (message: '~ts') "
		"with default settings, whose report is: ~w.",
		[ FirstMessage, FirstMultiSMSReport ] ),

	SecondMultiSMSReport = mobile:send_multipart_sms( SecondMessage,
													  MobileNumber ),

	test_facilities:display( "~nSent second multipart SMS (message: '~ts') "
		"with default settings, whose report is: ~w.",
		[ SecondMessage, SecondMultiSMSReport ] ),

	EncodingMultiTestFormatMsg = "This is a text sent in ~ts: aéàùâêîôû; "
		"this is a longer message meant *not* to be truncated, thanks to the "
		"multipart SMS feature.~n"
		"On a clear disk you can seek forever.~n"
		"$3,000,000. CHUBBY CHECKER just had a CHICKEN SANDWICH in downtown "
		"DULUTH!~n"
		"... the MYSTERIANS are in here with my CORDUROY SOAP DISH!! "
		"No one ever built a statue to a critic.~n"
		"Contains no artificial colors or ingredients. "
		"355/113 -- Not the famous irrational number PI, but an incredible "
		"simulation!~n"
		"I do desire we may be better strangers. "
		"  -- William Shakespeare, As You Like It.~n"
		"Put a rogue in the limelight and he will act like an honest man. "
		" -- Napoleon Bonaparte, Maxims.~n"
		"It's not whether you win or lose but how you played the game. "
		"  -- Grantland Rice. STOP.",

	GSMMultiUncompMsg = text_utils:format( EncodingMultiTestFormatMsg,
										   [ "GSM uncompressed" ] ),

	%test_facilities:display( "Will be sending now following message: '~ts'.",
	%						 [ GSMMultiUncompMsg ] ),

	GSMMultiUncompSMSReport = mobile:send_multipart_sms( GSMMultiUncompMsg,
										MobileNumber, Class, gsm_uncompressed ),

	test_facilities:display( "~nSent multipart SMS (message: '~ts') "
		"for the test of GSM uncompressed encoding, whose report is: ~w.",
		[ GSMMultiUncompMsg, GSMMultiUncompSMSReport ] ),



	UnicodeMultiUncompMsg = text_utils:format( EncodingMultiTestFormatMsg,
											   [ "Unicode uncompressed" ] ),

	%test_facilities:display( "Sending now: '~ts'.",
	%    [ UnicodeMultiUncompMsg ] ),

	UnicodeMultiUncompSMSReport = mobile:send_multipart_sms(
		UnicodeMultiUncompMsg, MobileNumber, Class, unicode_uncompressed ),

	test_facilities:display( "~nSent multipart SMS (message: '~ts') "
		"for the test of Unicode uncompressed encoding, whose report is: ~w.",
		[ UnicodeMultiUncompMsg, UnicodeMultiUncompSMSReport ] ),



	AutoMultiMsg = text_utils:format( EncodingMultiTestFormatMsg,
									  [ "automatic mode" ] ),

	%test_facilities:display( "Sending now: '~ts'.", [ AutoMsg ] ),

	AutoMultiSMSReport = mobile:send_multipart_sms( AutoMultiMsg,
													MobileNumber ),

	test_facilities:display( "~nSent multipart SMS (message: '~ts') "
		"for the test of (automatic) encoding, whose report is: ~w.",
		[ AutoMultiMsg, AutoMultiSMSReport ] ),


	%test_facilities:display( "Sending now the same message in full automatic "
	%						 "mode (regarding encoding and parts)..." ),

	FullSMSReport = mobile:send_sms( AutoMultiMsg, MobileNumber ),

	test_facilities:display( "~nSent message: '~ts' in full automatic mode, "
		"report is: ~w.", [ AutoMultiMsg, FullSMSReport ] ),

	% Automatic, single part, test for either encoding:
	mobile:send_sms( "Unicode expected: âêîôû.", MobileNumber ),

	mobile:send_sms( "GSM-encoding expected (end of test).", MobileNumber ).
