% Copyright (C) 2019-2021 Olivier Boudeville
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



% @doc Module implementing the <b>Ceylan-Mobile services</b>.
%
% Operates through a Seaplus-based interface to the Gammu backend library.
%
-module(mobile).



% API declaration.
%
% Note that the first functions below have a spec (and a pseudo edoc
% documentation), yet are not even defined here (Seaplus is take care of them
% automatically).


% API types:

-type backend_type() :: 'gammu'.

-type backend_version() :: basic_utils:version().
% Ex: {1, 40, 1}.


-type device_name() :: bin_string().

-type manufacturer_name() :: bin_string().

-type model_name() :: bin_string().

-type revision_text() :: bin_string().
-type date_text() :: bin_string().
-type revision_number() :: float().

-type imei() :: bin_string().
% International Mobile Equipment Identity code.
%
% Ex: <<"154483225355085">>.


-type hardware_info() :: bin_string().
% Ex: <<"FOO DUMMY BAR">>, <<"Gammu error: Function not supported by phone.">>.


-type imsi_code() :: bin_string().
% International Mobile Subscriber Identity code.
% Ex: <<"208150030213526">>.


-type signal_strength() :: integer_percent().
% In dBm. Ex: -51 dBm or 42 dBm.

-type signal_strength_percent() :: integer_percent().
% In dBm. Ex: 100% or 42%.


-type error_rate() :: integer_percent().
% Ex: -1% (!) or 0%.


-type sms_message() :: ustring().
% User-specified SMS message.


-type bin_sms_message() :: bin_string().


-type sms_class() :: non_neg_integer().
% Most SMS are of class 1 (the default, should no class by specified).
%
% See also [http://www.ozekisms.com/index.php?owpn=544].


-type mobile_number() :: ustring().
% The mobile number associated to a device (ex: "+1234567890").


-type bin_mobile_number() :: bin_string().
% The mobile number associated to a device, as a binary
% (ex: `<<"+1234567890">>').


-type encoding() :: % Default Unicode:
					'unicode_uncompressed'

				  | 'unicode_compressed'

					% Default GSM alphabet:
				  | 'gsm_uncompressed'

				  | 'gsm_compressed'

				  | 'eight_bit'.
% How the text of a SMS shall be encoded.


-type sms_sending_status() :: 'success' | 'failure'.
% Describes the status of a SMS sending.

-type sms_tpmr() :: basic_utils:count().
% Message reference, as generated by the GSM network.


-type sms_sending_report() :: { sms_sending_status(), sms_tpmr() }.
% Returned by a sending.


%-type sms_timestamp() :: time_utils:timestamp().
-type sms_timestamp() :: bin_string().


% For the received_sms record:
-include("mobile.hrl").


-type received_sms() :: #received_sms{}.
% Describes a received SMS.


-export_type([ backend_type/0, backend_version/0,
			   device_name/0, manufacturer_name/0,
			   model_name/0, revision_text/0,
			   date_text/0, revision_number/0, imei/0,
			   hardware_info/0, imsi_code/0,
			   signal_strength/0, signal_strength_percent/0,
			   error_rate/0,
			   sms_message/0, sms_class/0,
			   mobile_number/0, encoding/0,
			   sms_sending_status/0, sms_tpmr/0,
			   sms_sending_report/0, received_sms/0 ]).


% Exported helpers:
-export([ received_sms_to_string/1, get_execution_target/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type integer_percent() :: math_utils:integer_percent().


% For the Seaplus support (to be included after local exports):
-include_lib("seaplus/include/seaplus.hrl").

% To define get_execution_target/0:
-include_lib("myriad/include/utils/basic_utils.hrl").




% API functions:
%
% Ceylan-Mobile shall be started before any of them is triggered.

% Note that most functions have their implementation generated through Seaplus
% and thus are unfortunately invisible to edoc; their doc tags are thus
% intentionally non-standard ('doc:') to avoid that edoc fails.


% We list first the spec (with a doc-like description) of the functions that we
% leave as fully generated through Seaplus.



% doc: Tells whether Ceylan-Mobile can be used on this computer.
%
% Tells notably if the backend is available and is able to find a suitable
% configuration file; for example does not tell whether an actual device can be
% used (see has_actual_device/0 for that).
%
% Avoids the user code to have to trigger a dummy operation and catch a possible
% exception in order to know whether the backend is usable.
%
-spec is_available() -> boolean().


% doc: Returns the name of the (supposedly connected) mobile device.
%
% Ex: <<"/dev/ttyUSB-C3G">>, or <<"/tmp/gammu-dummy-device">>.
%
-spec get_device_name() -> device_name().


% doc: Returns the manufacturer of the (supposedly connected) mobile device.
%
% Ex: <<"Ericsson">>, or <<"Gammu">>.
%
-spec get_device_manufacturer() -> manufacturer_name().


% doc: Returns the model of the (supposedly connected) mobile device.
%
% Ex: <<"E19X">>, or <<"Dummy">>.
%
-spec get_device_model() -> model_name().


% doc: Returns the firmware information from the (supposedly connected) mobile
% device.
%
% Ex: {<<"11.104.20.01.00">>, <<"">>, 11.3042002}, or {<<"1.42.0">>,
% <<"20150101">>, 1.42}.
%
-spec get_firmware_information() ->
					{ revision_text(), date_text(), revision_number() }.


% doc: Tells whether a real (non-virtual, i.e. non-emulated) device is
% connected.
%
-spec has_actual_device() -> boolean().


% doc: Returns the IMEI/serial number of the (supposedly connected) mobile
% device.
%
-spec get_imei_code() -> imei().


% doc: Returns the hardware information reported by (supposedly connected)
% mobile device.
%
% Ex: <<"FOO DUMMY BAR">>, <<"Gammu error: Function not supported by phone.">>.
%
-spec get_hardware_information() -> hardware_info().


% doc: Returns the IMSI (<em>International Mobile Subscriber Identity</em>)
% code, typically stored in the SIM card.
%
-spec get_imsi_code() -> imsi_code().


% doc: Reads the current signal quality (strength and error rate).
%
% Typical value for signal strength is -51 dBm (100%) for an actual device, 42
% dBm (42%) for an emulated one.
%
% Note that the returned error rate might be -1% (actual) or 0% (emulated).
%
-spec get_signal_quality() ->
		{ signal_strength(), signal_strength_percent(), error_rate() }.





% API function overriding section.

% Then we list the functions that we chose to override specifically (these ones
% will be directly seen by edoc then).


% Key in the process dictionary allowing to keep the GSM charset in the context
% once for all:
%
-define( mobile_gsm_charset_key, "_mobile_gsm_charset" ).


% Key in the process dictionary allowing to keep the encoding conversion table
% in the context once for all:
%
-define( mobile_encoding_key, "_mobile_encoding_table" ).



% @doc Tells whether Ceylan-Mobile may be used on this computer; not supposed to
% crash ever.
%
% Tells notably if the backend is available and is able to find a suitable
% configuration file; for example does not tell whether an actual device can be
% used (see has_actual_device/0 for that).
%
% Avoids the user code to have to trigger a dummy operation and catch a possible
% exception in order to know whether the backend is usable.
%
-spec is_available() -> boolean().
is_available() ->

	% No better/simpler/more reliable test known that:
	try get_device_name() of

		_ ->
			cond_utils:if_defined( mobile_debug_driver, trace_utils:debug(
				"Mobile considered to be available." ) ),
			true

	% Intercepting just the exception class and pattern of interest:
	catch

		throw:{ driver_crashed, unknown_reason } ->

			cond_utils:if_defined( mobile_debug_driver, trace_utils:debug(
				"Mobile considered as not available, as cannot perform a "
				"Gammu test operation (driver crash)." ) ),

			false;

		% Never crashing:
		Class:Pattern ->

			trace_utils:error_fmt( "Unexpected exception raised "
				"when checking the availability of Gammu: "
				"exception class is ~p, pattern is ~p.", [ Class, Pattern ] ),

			false

	end.



% @doc Starts the Mobile service.
%
% We define our own service-specific starting procedure, knowing that a call to
% the corresponding Seaplus start will be automatically added, at first
% position, by the Seaplus parse transform.
%
-spec start() -> void().
start() ->
	start_common().


% @doc Starts and links the Mobile service.
-spec start_link() -> void().
start_link() ->
	start_common().



% (helper)
start_common() ->

	cond_utils:if_defined( mobile_debug_base,
						   trace_bridge:debug( "Starting Mobile." ) ),

	% This is needed whenever for example the overall (Erlang) application is
	% launched with the '-noinput' option (in this case the VM encoding switches
	% from unicode to latin1, and we cannot output proper UTF-8 characters
	% anymore: they are displayed as question marks in terminals):
	%
	io:setopts( [ { encoding, unicode } ] ),

	[ process_dictionary:put_as_new( K, V ) || { K, V } <-
		[ { ?mobile_gsm_charset_key, create_gsm_charset() },
		  { ?mobile_encoding_key, create_encoding_table() } ] ].



% @doc Returns a set containing all characters of the GSM, 7bit alphabet that
% shall not be escaped, for faster look-ups.
%
create_gsm_charset() ->

	% Based on
	% https://en.wikipedia.org/wiki/GSM_03.38#GSM_7-bit_default_alphabet_and_extension_table_of_3GPP_TS_23.038_/_GSM_03.38:

	set_utils:new(
		 [ C || C <- lists:seq( $a, $z ) ]
	  ++ [ C || C <- lists:seq( $A, $Z ) ]
	  ++ [ C || C <- lists:seq( $0, $9 ) ]
	  ++ [ $:, $;, $<, $=, $>, $?, $¡, $Ä, $Ö, $Ñ, $Ü, $§, $¿,
		   $ä, $ö, $ñ, $ü, $à, $@, $£, $$, $¥, $è, $é, $ù, $ì, $ò,
		   $Ç, $\n, $Ø, $ø, $\r, $Å, $å,
		   $Δ, $_, $Φ, $Γ, $Λ, $Ω, $Π, $Ψ, $Σ, $Θ, $Ξ,
		   % Removed as already expected to be escaped: $\\,
		   $Æ, $æ, $ß, $É, $, , $!, $", $#, $¤, $%, $&, $', $(, $),
		   $*, $+, $,, $-, $., $/ ] ).



% @doc Returns a suitable bijective table.
create_encoding_table() ->
	bijective_table:new( [ { unicode_uncompressed, 1 },
						   { unicode_compressed, 2 },
						   { gsm_uncompressed, 3 },
						   { gsm_compressed, 4 },
						   { eight_bit, 5 } ] ).



% @doc Returns hardware information regarding the (supposedly connected) mobile
% device.
%
% Throws an exception on failure, typically if the operation is not supported by
% the device.
%
% We override this function to throw an exception on failure, rather than for
% example sending tagged error tuples.
%
get_hardware_information() ->

	% These two pseudo-calls are replaced at compilation time by the Seaplus
	% parse transform with the relevant immediate values:

	PortKey = seaplus:get_service_port_key(),
	FunctionDriverId = seaplus:get_function_driver_id(),

	case seaplus:call_port_for( PortKey, FunctionDriverId, _Args=[] ) of

		Bin when is_binary( Bin ) ->
			Bin;

		Other ->
			throw( Other )

	end.



% @doc Returns the name and version of the backend used.
%
% We override this function for convenience: the C-side just returns the Gammu
% version as a string (ex: "1.40.0") and we use Myriad to easily convert it into
% {1,40,0}:
%
-spec get_backend_information() -> { backend_type(), backend_version() }.
get_backend_information() ->

	% These two pseudo-calls are replaced at compilation time by the Seaplus
	% parse transform with the relevant immediate values:

	PortKey = seaplus:get_service_port_key(),
	FunctionDriverId = seaplus:get_function_driver_id(),

	{ Backend, VersionString } =
		seaplus:call_port_for( PortKey, FunctionDriverId, _Args=[] ),

	% Overridding allows to perform a bit of post-processing here:
	VersionTuple = basic_utils:parse_version( VersionString ),

	{ Backend, VersionTuple }.



% doc: Tells whether a real (non-virtual, i.e. non-emulated) device is
% connected.
%
-spec has_actual_device() -> bool().
has_actual_device() ->

	% Best criterion:
	case get_device_manufacturer() of

		<<"Gammu">> ->
			false;

		_ ->
			true
	end.


% For the (key) sending of SMS, we override a lot the default Seaplus
% behaviours.


% @doc Sends specified, regular (that is non-multipart) SMS (of class 1), using
% an automatically-detected encoding.
%
% Returns whether it succeeded, and the message TPRM reference.
%
-spec send_regular_sms( sms_message(), mobile_number() ) ->
								sms_sending_report().
send_regular_sms( Message, MobileNumber ) ->
	send_regular_sms( Message, MobileNumber, _Class=1 ).



% @doc Sends a regular (non-multipart) SMS, using specified class and an
% automatically-detected encoding.
%
% Returns whether it succeeded, and the message TPRM reference.
%
-spec send_regular_sms( sms_message(), mobile_number(), sms_class() ) ->
								sms_sending_report().
send_regular_sms( Message, MobileNumber, Class ) ->

	% We directly branch to the more complete version, the only one to be known
	% of the driver:
	%
	{ ActualEncoding, ActualMessage } = case scan_characters( Message ) of

		{ single_sms, Encoding, ReadyMessage } ->

			%trace_bridge:debug_fmt( "Sending '~ts' as a single SMS, with "
			%    "encoding ~ts.", [ ReadyMessage, Encoding ] ),

			{ Encoding, ReadyMessage } ;


		{ multiple_sms, Encoding, ReadyMessage } ->

			%trace_bridge:warning_fmt(
			%  "Sending '~ts' as a single SMS (as requested), with "
			%  "encoding ~ts, yet expecting it to be truncated.",
			%  [ ReadyMessage, Encoding ] ),

			{ Encoding, ReadyMessage }

	end,

	send_regular_sms( ActualMessage, MobileNumber, Class, ActualEncoding ).



% @doc Sends a regular (non-multipart) SMS, using specified class and encoding.
%
% Returns whether it succeeded, and the message TPRM reference.
%
-spec send_regular_sms( sms_message(), mobile_number(), sms_class(),
						encoding() ) -> sms_sending_report().
send_regular_sms( Message, MobileNumber, Class, Encoding )
  when is_list( Message ) andalso is_list( MobileNumber )
	   andalso is_integer( Class ) andalso is_atom( Encoding ) ->

	% Only available directly in this (overridden) function:
	PortKey = seaplus:get_service_port_key(),
	FunctionDriverId = seaplus:get_function_driver_id(),

	% Exchanging binaries and directly numerical identifiers is more efficient:

	MessageBin = unicode:characters_to_binary( Message ),
	MobileNumberBin = text_utils:string_to_binary( MobileNumber ),
	EncodingEnum = encoding_to_enum( Encoding ),

	Args = [ MessageBin, MobileNumberBin, Class, EncodingEnum ],

	%trace_bridge:debug_fmt( "send_regular_sms/4 sending arguments ~p.",
	%						[ Args ] ),

	seaplus:call_port_for( PortKey, FunctionDriverId, Args ).



% @doc Sends a multipart SMS, using default class 1 and an
% automatically-detected encoding.
%
% Returns whether it succeeded, and the message TPRM reference.
%
-spec send_multipart_sms( sms_message(), mobile_number() ) ->
								sms_sending_report().
send_multipart_sms( Message, MobileNumber ) ->
	send_multipart_sms( Message, MobileNumber, _Class=1 ).



% @doc Sends a multipart SMS, using specified class and an
% automatically-detected encoding.
%
% Returns whether it succeeded, and the message TPRM reference.
%
-spec send_multipart_sms( sms_message(), mobile_number(), encoding() ) ->
								sms_sending_report().
send_multipart_sms( Message, MobileNumber, Class ) ->

	% We directly branch to the more complete version, the only one to be known
	% of the driver:
	%
	{ ActualEncoding, ActualMessage } = case scan_characters( Message ) of

		{ single_sms, Encoding, ReadyMessage } ->

			%trace_bridge:warning_fmt(
			%  "Sending '~ts' as a multipart SMS (as requested), with "
			%  "encoding ~ts, yet believing a single-part SMS would have "
			%  "sufficed.", [ ReadyMessage, Encoding ] ),

			{ Encoding, ReadyMessage } ;


		{ multiple_sms, Encoding, ReadyMessage } ->

			%trace_bridge:debug_fmt( "Sending '~ts' as a multipart SMS, with "
			%    "encoding ~ts.", [ ReadyMessage, Encoding ] ),

			{ Encoding, ReadyMessage }

	end,

	send_multipart_sms( ActualMessage, MobileNumber, Class, ActualEncoding ).



% @doc Sends specified SMS, using specified class and encoding.
%
% Returns whether it succeeded, and the message TPRM reference.
%
-spec send_multipart_sms( sms_message(), mobile_number(), sms_class(),
						  encoding() ) -> sms_sending_report().
send_multipart_sms( Message, MobileNumber, Class, Encoding )
  when is_list( Message ) andalso is_list( MobileNumber )
	   andalso is_integer( Class ) andalso is_atom( Encoding ) ->

	% Only available directly in this (overridden) function:
	PortKey = seaplus:get_service_port_key(),
	FunctionDriverId = seaplus:get_function_driver_id(),

	% Exchanging binaries and identifiers is more efficient:

	MessageBin = unicode:characters_to_binary( Message ),
	MobileNumberBin = text_utils:string_to_binary( MobileNumber ),
	EncodingEnum = encoding_to_enum( Encoding ),

	Args = [ MessageBin, MobileNumberBin, Class, EncodingEnum ],

	%trace_bridge:debug_fmt( "send_multipart_sms/4 sending arguments ~p.",
	%						[ Args ] ),

	seaplus:call_port_for( PortKey, FunctionDriverId, Args ).



% @doc Sends specified SMS (of class 1), determining automatically the best
% encoding to use, and whether a regular SMS or a multipart one is needed.
%
% Returns whether it succeeded, and the message TPRM reference.
%
% The most advanced SMS-sending primitive, switching automatically to the right
% lower-level one, for the default class 1.
%
-spec send_sms( sms_message(), mobile_number() ) -> sms_sending_report().
send_sms( Message, MobileNumber ) ->
	send_sms( Message, MobileNumber, _Class=1 ).



% @doc Sends specified SMS, of specified class, determining automatically the
% best encoding to use, and whether a regular SMS or a multipart one is needed.
%
% The most advanced SMS-sending primitive, switching automatically to the right
% lower-level one, based on specified class.
%
-spec send_sms( sms_message(), mobile_number(), sms_class() ) ->
						sms_sending_report().
send_sms( Message, MobileNumber, Class ) ->

	% Select the right sending primitive to call:
	case scan_characters( Message ) of

		{ single_sms, Encoding, ReadyMessage } ->

			%trace_bridge:debug_fmt( "Sending '~ts' as a single SMS, with "
			%   "class ~B and encoding ~ts.",
			%   [ ReadyMessage, Class, Encoding ] ),

			send_regular_sms( ReadyMessage, MobileNumber, Class, Encoding );


		{ multiple_sms, Encoding, ReadyMessage } ->

			%trace_bridge:debug_fmt( "Sending '~ts' as a multipart SMS, with "
			%   "class ~B and encoding ~ts.",
			%   [ ReadyMessage, Class, Encoding ] ),

			send_multipart_sms( ReadyMessage, MobileNumber, Class, Encoding )

	end.


% (helper)
scan_characters( Message ) ->

	GSMCharSet = process_dictionary:get_existing( ?mobile_gsm_charset_key ),

	scan_characters( Message, _GSMUCharCount=0, _UCS2UCharCount=0,
		_CurrentEncoding=gsm_uncompressed, _GSMUMessage=[],
		_UCS2UMessage=Message, GSMCharSet ).


% (sub-helper)
scan_characters( _Message=[], GSMUCharCount, _UCS2UCharCount,
		CurrentEncoding=gsm_uncompressed, GSMUMessage, _UCS2UMessage,
		_GSMCharSet ) ->

	% Can only be decide once all characters have been examined (as even the
	% last one may be a Unicode one):
	%
	SMSMultiplicity = case GSMUCharCount > 160  of

		true ->
			multiple_sms;

		false ->
			single_sms

	 end,

	{ SMSMultiplicity, CurrentEncoding, lists:reverse( GSMUMessage ) };


scan_characters( _Message=[], _GSMUCharCount, _UCS2UCharCount,
		CurrentEncoding=unicode_uncompressed, _GSMUMessage, UCS2UMessage,
		_GSMCharSet ) ->
	%  If not having exit beforehand, it means:
	{ single_sms, CurrentEncoding, UCS2UMessage };


scan_characters( _Message=[ C | H ], GSMUCharCount, UCS2UCharCount,
		CurrentEncoding=gsm_uncompressed, GSMUMessage, UCS2UMessage,
		GSMCharSet ) ->

	% With the default GSM alphabet, some characters have to be escaped:
	case lists:member( C, [ $|, $^, $€, ${, $}, $[, $], $\\ ] ) of

		true ->
			% Still the default GSM alphabet, yet must be escaped then:
			scan_characters( H, GSMUCharCount+2, UCS2UCharCount+1,
				CurrentEncoding, [ C, $\ | GSMUMessage ], UCS2UMessage,
				GSMCharSet );

		false ->
			% Either belonging to the unescaped default GSM alphabet, or to the
			% UCS-2 one (the actual encoding will be done by Gammu, here we just
			% determine the right encoding and single/multipart settings to
			% select):
			%
			case is_gsm_char( C, GSMCharSet ) of

				true ->
					scan_characters( H, GSMUCharCount+1, UCS2UCharCount+1,
						CurrentEncoding, [ C | GSMUMessage ],
						UCS2UMessage, GSMCharSet );

				false ->
					% Alphabet switch required, no need to take care of GSM
					% anymore, we just have to determine next whether a single
					% or multipart SMS is needed then:
					%
					scan_characters( H, _GSMUCharCount=0, UCS2UCharCount+1,
						unicode_uncompressed, _GSMUMessage=[],
						UCS2UMessage, GSMCharSet )

			end

	end;


% Shortcut (regardless of the next characters, we will stick to multipart
% UCS-2):
%
%scan_characters( _Message=[ C | H ], GSMUCharCount, UCS2UCharCount,
scan_characters( _Message, _GSMUCharCount, UCS2UCharCount,
		CurrentEncoding=unicode_uncompressed, _GSMUMessage,
		UCS2UMessage, _GSMCharSet ) when UCS2UCharCount > 70 ->

	% No need to go further:
	{ multiple_sms, CurrentEncoding, UCS2UMessage };

scan_characters( _Message=[ _C | H ], _ZeroGSMUCharCount, UCS2UCharCount,
		CurrentEncoding=unicode_uncompressed, _EmptyGSMUMessage,
		UCS2UMessage, GSMCharSet ) ->

	% No need to take care of GSM anymore:
	scan_characters( H, _StillZeroGSMUCharCount=0, UCS2UCharCount+1,
		CurrentEncoding, _StillEmptyGSMUMessage=[], UCS2UMessage, GSMCharSet ).



% @doc Tells whether specified character may be encoded in the default GSM
% non-espaced alphabet:
%
% We were initially considering to rely on a well-crafted list, however a set is
% by far more appropriate here.
%
% We used to try to favour ranges over cherry-picked codes, and roughly from the
% most frequent characters to the least:
%
% @end
%
%is_gsm_char( C ) when C >= $a andalso C =< $z->
%	true;
%
%is_gsm_char( C ) when C >= $0 andalso C =< $9->
%	true;
%
%is_gsm_char( C ) when C >= $A andalso C =< $Z->
%	true;
%
%is_gsm_char( C ) ->

	% Remaining subsets:
	%
	%    $: $; $< $= $> $? $¡
	%    $Ä $Ö $Ñ $Ü $§ $¿
	%    $ä $ö $ñ $ü $à
	%    $@ $£ $$ $¥ $è $é $ù $ì $ò $Ç $\n $Ø $ø $\r $Å $å
	%    $Δ $_ $Φ $Γ $Λ $Ω $Π $Ψ $Σ $Θ $Ξ $\\ $Æ $æ $ß $É
	%    $  $! $" $# $¤ $% $& $' $( $) $* $+ $, $- $. $/
	%
	% We remove $\\ as it is already escaped, and reorder characters from
	% (approximately) most frequent to least:
	%
	%    $  $: $; $( $) $* $! $? $+ $, $- $. $\n $\r $% $& $' $/ $_
	%    $" $# $@ $£ $$ $¥ $è $é $ù $< $= $>
	%    $à $É $Ä $Ö $Ñ $Ü $§ $¿ $¡
	%    $ä $ö $ñ $ü
	%    $ì $ò $Ç $Ø $ø $Å $å
	%    $Δ $Φ $Γ $Λ $Ω $Π $Ψ $Σ $Θ $Ξ $Æ $æ $ß $¤
	%
	%
	% See also:
	% [http://erlang.org/doc/reference_manual/data_types.html#escape-sequences]

is_gsm_char( C, GSMCharset ) ->
	set_utils:member( C, GSMCharset ).




% (helper; see the enum encoding in the corresponding driver)
encoding_to_enum( Encoding ) ->
	Table = process_dictionary:get( ?mobile_encoding_key ),
	bijective_table:get_second_for( Encoding, Table ).


% Reverse conversion:
enum_to_encoding( Value ) ->
	Table = process_dictionary:get( ?mobile_encoding_key ),
	bijective_table:get_first_for( Value, Table ).



% @doc Reads all SMS already received (if any), and, if true is specified,
% deletes them as soon as they are read.
%
% Does not block.
%
% Specialised here to transform conveniently its outputs.
%
-spec read_all_sms( boolean() ) -> [ received_sms() ].
read_all_sms( DeleteOnReading ) ->

	% These two pseudo-calls are replaced at compilation time by the Seaplus
	% parse transform with the relevant immediate values:

	PortKey = seaplus:get_service_port_key(),
	FunctionDriverId = seaplus:get_function_driver_id(),

	DeleteToggle = case DeleteOnReading of

		true ->
			1;

		false ->
			0

	end,


	SMSList = case seaplus:call_port_for( PortKey, FunctionDriverId,
										  _Args=[ DeleteToggle ] ) of

		L when is_list( L ) ->
			L;

		Other ->
			throw( { faulty_read_return, Other } )

	end,

	[ to_sms( E ) || E <- SMSList ].



% @doc Converts a transmitted subset of GSM_SMSMessage into a received_sms
% record.
%
% (helper)
%
to_sms( { BinSenderNumber, EncodingValue, MessageReference, Timestamp,
		  BinText } ) ->
	#received_sms{ sender_number=BinSenderNumber,
				   encoding=enum_to_encoding( EncodingValue ),
				   text=BinText,
				   message_reference=MessageReference,
				   timestamp=Timestamp }.



% @doc Returns an overall, textual information about the current mobile setting.
-spec get_textual_information() -> ustring().
get_textual_information() ->

	{ BackendType, BackendVersion } = get_backend_information(),

	HardwareInfoBin = get_hardware_information(),

	{ BinRevText, BinDataText, FloatRev } = get_firmware_information(),

	{ SigStrength, SigStrPct, ErrorRate } = get_signal_quality(),

	text_utils:format( "Ceylan-Mobile based on the ~ts version of "
		"the ~ts backend, running on '~ts' hardware (IMEI: '~ts'). "
		"Device name is '~ts', manufacturer is '~ts', model is '~ts'. "
		"Firmware is revision '~ts' (date: '~ts', number: ~f). "
		"IMSI is '~ts'. "
		"Signal strength is ~B dBm (~B%), error rate is ~B%",
		[ text_utils:version_to_string( BackendVersion ), BackendType,
		  HardwareInfoBin, get_imei_code(),
		  get_device_name(), get_device_manufacturer(), get_device_model(),
		  BinRevText, BinDataText, FloatRev,
		  get_imsi_code(), SigStrength, SigStrPct, ErrorRate ] ).



% @doc Returns a textual description of the specified received SMS.
received_sms_to_string( #received_sms{ sender_number=Number,
									   encoding=Encoding,
									   text=Text,
									   message_reference=MsgRef,
									   timestamp=Timestamp } ) ->
	text_utils:format( "received SMS sent from number '~ts' (with encoding ~ts)"
		 "whose text is: '~ts' (reference: ~p, sending timestamp: ~ts)",
		[ Number, Encoding, Text, MsgRef,
		  time_utils:timestamp_to_string( Timestamp ) ] ).



% @doc Stops the Mobile service.
%
% Service-specific stop procedure.
%
-spec stop() -> void().
stop() ->

	cond_utils:if_defined( mobile_debug_base,
						   trace_bridge:debug( "Stopping Mobile." ) ),
	[ process_dictionary:remove_existing( K )
		|| K <- [ ?mobile_gsm_charset_key, ?mobile_encoding_key ] ].
