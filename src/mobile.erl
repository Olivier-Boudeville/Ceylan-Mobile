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


% For the Seaplus support:
-include("seaplus.hrl").



% API declaration.
%
% Note that the functions below have a spec, yet are not even defined here.



% API types:


-type backend_type() :: 'gammu'.

-type backend_version() :: text_utils:ustring().


-type device_name() :: text_utils:bin_string().

-type manufacturer_name() :: text_utils:bin_string().

-type model_name() :: text_utils:bin_string().

-type revision_text() :: text_utils:bin_string().
-type date_text() :: text_utils:bin_string().
-type revision_number() :: float().

-type imei() :: text_utils:bin_string().

-type hardware_info() :: text_utils:bin_string().


% International Mobile Subscriber Identity code:
-type imsi_code() :: text_utils:bin_string().


% In dBm:
-type signal_strength() :: math_utils:integer_percent().

-type signal_strength_percent() :: math_utils:integer_percent().


-type error_rate() :: math_utils:integer_percent().


% User-specified SMS message:
-type sms_message() :: text_utils:string().

% The mobile number associated to a device (ex: "+1234567890"):
-type mobile_number() :: text_utils:string().


% How the text of a SMS shall be encoded:
-type encoding() :: % Default Unicode:
					'unicode_uncompressed'

				  | 'unicode_compressed'

					% Default GSM alphabet:
				  | 'gsm_uncompressed'

				  | 'gsm_compressed'

				  | 'eight_bit'.


% Describes the status of a SMS sending:
-type sms_sending_status() :: 'success' | 'failure'.

% Message reference, as generated by the GSM network:
-type sms_tpmr() :: basic_utils:count().

% Returned by a sending:
-type sms_sending_report() :: { sms_sending_status(), sms_tpmr() }.



% API functions:


% Returns the name and version of the backend used.
-spec get_backend_information() -> { backend_type(), backend_version() }.


% Returns the name of the (supposedly connected) mobile device.
-spec get_device_name() -> device_name().


% Returns the manufacturer of the (supposedly connected) mobile device.
-spec get_device_manufacturer() -> manufacturer_name().


% Returns the model of the (supposedly connected) mobile device.
-spec get_device_model() -> model_name().


% Returns the firmware information from the (supposedly connected) mobile
% device.
%
-spec get_firmware_information() ->
				   { revision_text(), date_text(), revision_number() }.


% Returns the IMEI/serial number of the (supposedly connected) mobile device.
-spec get_imei_code() -> imei().


% Returns hardware information about the (supposedly connected) mobile device.
%
% Throws an exception on failure, typically if the operation is not supported by
% the device.
%
-spec get_hardware_information() -> hardware_info().


% Returns the IMSI (International Mobile Subscriber Identity) code, typically
% stored in the SIM card.
%
-spec get_imsi_code() -> imsi_code().


% Reads the current signal quality (strength and error rate).
-spec get_signal_quality() ->
		 { signal_strength(), signal_strength_percent(), error_rate() }.


% Sends specified SMS.
%
% Returns whether it succeeded, and the message TPRM reference.
%
-spec send_sms( sms_message(), mobile_number() ) -> sms_sending_report().



% Sends specified SMS, using specified encoding.
%
% Returns whether it succeeded, and the message TPRM reference.
%
-spec send_sms( sms_message(), mobile_number(), encoding() ) ->
					  sms_sending_report().



% API function overriding section.


% We define our own service-specific starting procedure, knowing that a call to
% the corresponding Seaplus start will be automatically added afterwards.
%
start() ->

	% This is needed whenever for example the overall (Erlang) application is
	% launched with the -noinput option (in this case the VM encoding switches
	% from unicode to latin1, and we cannot output proper UTF-8 characters
	% anymore (they are displayed as question marks in terminals):
	%
	io:setopts( [ { encoding, unicode } ] ).


start_link() ->
	% Same needs:
	start().


% We override this function to throw an exception on failure, rather than
% sending tagged tuples for example.
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



% We override this function for convenience: the C-side just returns the Gammu
% version as a string (ex: "1.40.0") and we use Myriad to easily convert it into
% {1,40,0}:
%
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



% Exchanging binaries is more efficient.
send_sms( Message, MobileNumber ) ->

	PortKey = seaplus:get_service_port_key(),
	FunctionDriverId = seaplus:get_function_driver_id(),

	Args = text_utils:strings_to_binaries( [ Message, MobileNumber ] ),

	seaplus:call_port_for( PortKey, FunctionDriverId, Args ).



% Exchanging binaries and identifiers is more efficient.
send_sms( Message, MobileNumber, Encoding ) ->

	PortKey = seaplus:get_service_port_key(),
	FunctionDriverId = seaplus:get_function_driver_id(),

	MessageBin = unicode:characters_to_binary( Message ),
	MobileNumberBin = text_utils:string_to_binary( MobileNumber ),
	EncodingEnum = encoding_to_enum( Encoding ),

	Args = [ MessageBin, MobileNumberBin, EncodingEnum ],

	seaplus:call_port_for( PortKey, FunctionDriverId, Args ).



% (helper)
encoding_to_enum( unicode_uncompressed ) ->
	1;

encoding_to_enum( unicode_compressed ) ->
	2;

encoding_to_enum( gsm_uncompressed ) ->
	3;

encoding_to_enum( gsm_compressed ) ->
	4;

encoding_to_enum( eight_bit ) ->
	5.


% No need to define a specific stop/0 here.
