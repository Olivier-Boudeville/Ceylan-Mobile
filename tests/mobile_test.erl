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
% Some test sentences emanate from dear /usr/bin/fortune
%
% Note that with the 'dummy' Gammu model, TPMR references might be always 255.
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


	% This operation may not be supported by the end device:
	try

		test_facilities:display( "Hardware information: '~s'.",
								 [ mobile:get_hardware_information() ] )

	catch

		HardwareInfoException ->
			test_facilities:display( "Failed to get hardware information: ~p",
									 [ HardwareInfoException ] )

	end,

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

	test_facilities:display( "~n~nThe next sending tests will target the "
							 "following recipient mobile number: '~s', first "
							 "with a few single-part SMS, of various lengths, "
							 "needing various encodings.", [ MobileNumber ] ),


	% Single (non-multipart) SMS testing:

	% Of course short enough not to be truncated:
	FirstMessage = "Hello world!",

	FirstSMSReport = mobile:send_regular_sms( FirstMessage, MobileNumber ),

	test_facilities:display( "~nSent first (single-part) SMS (message: '~s') "
							 "with default settings, whose report is: ~w.",
							 [ FirstMessage, FirstSMSReport ] ),


	% With (uncompressed) GSM encoding, the single received SMS should stop just
	% after the second 'Tom':
	%
	SecondMessage = text_utils:format(
		"Goodbye! This is a longer SMS to test their support. "
		"For thee the wonder-working earth puts forth sweet flowers. "
		"-- Titus Lucretius Carus~n"
		"Ho! Tom Bombadil, Tom Bombadillo! "
		"By water, wood and hill, by reed and willow, "
		"By fire, sun and moon, harken now and hear us!"
		"Come, Tom Bombadil, for our need is near us! "
		"-- J. R. R. Tolkien", [] ),

	SecondSMSReport = mobile:send_regular_sms( SecondMessage, MobileNumber ),

	test_facilities:display( "~nSent second (single-part) SMS (message: '~s') "
							 "with default settings, whose report is: ~w.",
							 [ SecondMessage, SecondSMSReport ] ),


	EncodingTestFormatMsg = "This is a text sent in ~ts: aéàùâêîôû; "
		"this is a longer message meant to be truncated should a single SMS "
		"be used (instead of a multi-part one, involving multiple actual SMS "
		"that are to be collected and reassembled by the end device.~n"
		"If it happens once, it's a bug. If it happens twice, it's a feature. "
		"If it happens more than twice, it's a design philosophy. "
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
												  gsm_uncompressed ),

	test_facilities:display( "~nSent (single-part) SMS (message: '~s') for the "
							 "test of GSM uncompressed encoding, "
							 "whose report is: ~w.",
							 [ GSMUncompMsg, GSMUncompSMSReport ] ),



	UnicodeUncompMsg = text_utils:format( EncodingTestFormatMsg,
										 [ "Unicode uncompressed" ] ),

	%test_facilities:display( "Sending now: '~ts'.", [ UnicodeUncompMsg ] ),

	% With Unicode, "âêîôû" is expected to be received just fine, but the length
	% of this single SMS shall be quite small: stopping after "this is a long":
	%
	UnicodeUncompSMSReport = mobile:send_regular_sms( UnicodeUncompMsg,
								   MobileNumber, unicode_uncompressed ),

	test_facilities:display( "~nSent (single-part) SMS (message: '~s') for the "
							 "test of Unicode uncompressed encoding, "
							 "whose report is: ~w.",
							 [ UnicodeUncompMsg, UnicodeUncompSMSReport ] ),


	AutoMsg = text_utils:format( EncodingTestFormatMsg,
								 [ "automatic encoding selection mode" ] ),

	%test_facilities:display( "Sending now: '~ts'.", [ AutoMsg ] ),

	AutoSMSReport = mobile:send_regular_sms( AutoMsg, MobileNumber ),

	test_facilities:display( "~nSent (single-part) SMS (message: '~s') for the "
							 "test of (automatic) encoding, "
							 "whose report is: ~w.",
							 [ AutoMsg, AutoSMSReport ] ),



	% Now, multipart testing:

	test_facilities:display( "~n~nNow sending the same kind of messages, "
							 "this time using multipart SMS." ),


	FirstMultiSMSReport = mobile:send_multipart_sms( FirstMessage, MobileNumber ),

	test_facilities:display( "~nSent first multipart SMS (message: '~s') "
							 "with default settings, whose report is: ~w.",
							 [ FirstMessage, FirstMultiSMSReport ] ),

	SecondMultiSMSReport = mobile:send_multipart_sms( SecondMessage,
													  MobileNumber ),

	test_facilities:display( "~nSent second multipart SMS (message: '~s') "
							 "with default settings, whose report is: ~w.",
							 [ SecondMessage, SecondMultiSMSReport ] ),

	EncodingMultiTestFormatMsg = "This is a text sent in ~ts: aéàùâêîôû; "
		"this is a longer message meant *not* to be truncated, thanks to the "
		"multipart SMS feature.~n"
		"On a clear disk you can seek forever. "
		"$3,000,000. CHUBBY CHECKER just had a CHICKEN SANDWICH in downtown "
		"DULUTH! "
		"... the MYSTERIANS are in here with my CORDUROY SOAP DISH!! "
		"No one ever built a statue to a critic. "
		"Contains no artificial colors or ingredients. "
		"355/113 -- Not the famous irrational number PI, but an incredible "
		"simulation! "
		"I do desire we may be better strangers. "
		"  -- William Shakespeare, As You Like It "
		"Put a rogue in the limelight and he will act like an honest man. "
		" -- Napoleon Bonaparte, Maxims "
		"It's not whether you win or lose but how you played the game. "
		"  -- Grantland Rice. STOP.",

	GSMMultiUncompMsg = text_utils:format( EncodingMultiTestFormatMsg,
										   [ "GSM uncompressed" ] ),

	%test_facilities:display( "Will be sending now following message: '~ts'.",
	%						 [ GSMMultiUncompMsg ] ),

	GSMMultiUncompSMSReport = mobile:send_multipart_sms( GSMMultiUncompMsg,
									   MobileNumber, gsm_uncompressed ),

	test_facilities:display( "~nSent multipart SMS (message: '~s') "
							 "for the test of GSM uncompressed encoding, "
							 "whose report is: ~w.",
							 [ GSMMultiUncompMsg, GSMMultiUncompSMSReport ] ),



	UnicodeMultiUncompMsg = text_utils:format( EncodingMultiTestFormatMsg,
											   [ "Unicode uncompressed" ] ),

	%test_facilities:display( "Sending now: '~ts'.", [ UnicodeMultiUncompMsg ] ),

	UnicodeMultiUncompSMSReport =
		mobile:send_multipart_sms( UnicodeMultiUncompMsg, MobileNumber,
								   unicode_uncompressed ),

	test_facilities:display( "~nSent multipart SMS (message: '~s') "
							 "for the test of Unicode uncompressed encoding, "
							 "whose report is: ~w.", [ UnicodeMultiUncompMsg,
											 UnicodeMultiUncompSMSReport ] ),



	AutoMultiMsg = text_utils:format( EncodingMultiTestFormatMsg,
									  [ "automatic mode" ] ),

	%test_facilities:display( "Sending now: '~ts'.", [ AutoMsg ] ),

	AutoMultiSMSReport = mobile:send_multipart_sms( AutoMultiMsg, MobileNumber ),

	test_facilities:display( "~nSent multipart SMS (message: '~s') "
							 "for the test of (automatic) encoding, "
							 "whose report is: ~w.",
							 [ AutoMultiMsg, AutoMultiSMSReport ] ),


	%test_facilities:display( "Sending now the same message in full automatic "
	%						 "mode (regarding encoding and parts)..." ),

	FullSMSReport = mobile:send_sms( AutoMultiMsg, MobileNumber ),

	test_facilities:display( "~nSent message: '~s' in full automatic mode, "
							 "report is: ~w.",
							 [ AutoMultiMsg, FullSMSReport ] ).
