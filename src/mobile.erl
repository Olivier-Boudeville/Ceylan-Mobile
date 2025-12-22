% Copyright (C) 2019-2026 Olivier Boudeville
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

-module(mobile).

-moduledoc """
Module implementing the **Ceylan-Mobile services**, in charge of controlling
mobile devices (smartphones, dongles), 2G/3G, possibly 4G or more recent.

Refer to `send_sms/2` / `send_sms_multi/2` for the most useful, integrated
sending functions (with single/multiple recipients), and `read_all_sms/1` to
read any received SMSs.

Operates through a Seaplus-based interface to the Gammu backend library.

See Ceylan-Seaplus at [http://seaplus.esperide.org].
""".




% API declaration.
%
% Note that the first functions below have a spec (and a pseudo edoc
% documentation), yet are not even defined here (Seaplus is take care of them
% automatically).


% API types:

-type backend_type() :: 'gammu'.


-doc "For example `{1,40,1}`.".
-type backend_version() :: basic_utils:version().


-type device_name() :: bin_string().

-type manufacturer_name() :: bin_string().

-type model_name() :: bin_string().

-type revision_text() :: bin_string().
-type date_text() :: bin_string().
-type revision_number() :: float().



-doc """
International Mobile Equipment Identity code.

For example `<<"154483225355085">>`.
""".
-type imei() :: bin_string().



-doc """
For example `<<"FOO DUMMY BAR">>`, `<<"Gammu error: Function not supported by
phone.">>`.
""".
-type hardware_info() :: bin_string().



-doc """
International Mobile Subscriber Identity code.

For example `<<"208150030213526">>`.
""".
-type imsi_code() :: bin_string().



-doc "In dBm. For example -51 dBm or 42 dBm.".
-type signal_strength() :: integer_percent().



-doc "In dBm. For example 100% or 42%.".
-type signal_strength_percent() :: integer_percent().



-doc "For example -1% (!) or 0%.".
-type error_rate() :: integer_percent().



-doc "User-specified SMS message.".
-type sms_message() :: ustring().


-type bin_sms_message() :: bin_string().

-type any_sms_message() :: any_string().



-doc """
Most SMS are of class 1 ("normal", the default, should no class be specified).

See also [http://www.ozekisms.com/index.php?owpn=544].
""".
-type sms_class() :: 0 % Flash
                   | 1 % Normal
                   | non_neg_integer().



-doc """
A phone number (mobile or not), preferably international, preferably with no
whitespaces (e.g. `"+330616XXXXXX"`), as a plain string.
""".
-type phone_number() :: sms_utils:phone_number().


-doc """
A phone number (mobile or not), preferably international, preferably with no
whitespaces (e.g. `<<"+330616XXXXXX">>`), as a binary string.
""".
-type bin_phone_number() :: sms_utils:bin_phone_numbe().


-doc """
A phone number (mobile or not), preferably international, preferably with no
whitespaces (e.g. `<<"+330616XXXXXX">>`), as any string.
""".
-type any_phone_number() :: sms_utils:any_phone_numbe().


-doc """
The mobile number associated to a device, as a plain string
(e.g. `"+1234567890"`).
""".
-type mobile_number() :: phone_number().



-doc """
The mobile number associated to a device, as a binary string
(e.g. `<<"+1234567890">>`).
""".
-type bin_mobile_number() :: bin_phone_number().


-doc """
The mobile number associated to a device, as any string
(e.g. `"+1234567890"`).
""".
-type any_mobile_number() :: any_phone_number().




-doc "How the text of a SMS shall be encoded.".
-type encoding() ::

    % Default Unicode:
    'unicode_uncompressed' % Perfect encoding expected

  | 'unicode_compressed' % Not supported by most recipients anyway?

    % Default GSM alphabet:
  | 'gsm_uncompressed' % Some characters are correctly encoded (e.g. aéàù),
                       % some not (e.g. âêîôû become aeiou)

  | 'gsm_compressed' % Not supported by most recipients anyway?

  | 'eight_bit'.


-doc "Describes the status of a SMS sending.".
-type sms_sending_status() :: 'send_success' | 'send_failure'.



-doc "Message reference, as generated by the GSM network.".
-type sms_tpmr() :: pos_integer().



-doc "Returned by a sending.".
-type sms_sending_report() :: { sms_sending_status(), sms_tpmr() }.


-type sms_timestamp() :: time_utils:timestamp().



% For the received_sms record:
-include("mobile.hrl").



-doc """
Describes a received SMS.

More precisely corresponds to a multi-SMS message, each of these SMS having its
parts automatically aggregated.
""".
-type received_sms() :: #received_sms{}.


-export_type([ backend_type/0, backend_version/0,
               device_name/0, manufacturer_name/0,
               model_name/0, revision_text/0,
               date_text/0, revision_number/0, imei/0,
               hardware_info/0, imsi_code/0,
               signal_strength/0, signal_strength_percent/0,
               error_rate/0,
               sms_message/0, bin_sms_message/0, any_sms_message/0,
               sms_class/0,

               phone_number/0, bin_phone_number/0, any_phone_number/0,
               mobile_number/0, bin_mobile_number/0, any_mobile_number/0,

               encoding/0,
               sms_sending_status/0, sms_tpmr/0,
               sms_sending_report/0, sms_timestamp/0, received_sms/0 ]).


% Version-related functions.
-export([ get_mobile_version/0, get_mobile_version_string/0 ]).

% Exported helpers:
-export([ received_sms_to_string/1 ]).


% Local types:

-doc "As received from the C node.".
-type received_sms_tuple() :: { BinSenderNumber :: bin_phone_number(),
                                EncodingValue :: encoding_enum(),
                                MessageReference :: sms_tpmr(),
                                Timestamp :: sms_timestamp(),
                                BinText :: bin_string() }.

% Silencing:
-export_type([ received_sms_tuple/0 ]).


-type encoding_enum() :: pos_integer().

-type encoding_table() :: bijective_table( encoding(), encoding_enum() ).


% Type shorthands:

-type three_digit_version() :: basic_utils:three_digit_version().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type integer_percent() :: math_utils:integer_percent().

-type bijective_table( F, S ) :: bijective_table:bijective_table( F, S ).

-type service_key() :: seaplus:service_key().
-type function_driver_id() :: seaplus:function_driver_id().
-type function_params() :: seaplus:function_params().


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


% doc: Returns the name of the (supposedly connected) mobile device.
%
% For example `<<"/dev/ttyUSB-C3G">>`, or `<<"/tmp/gammu-dummy-device">>`.
%
-spec get_device_name() -> device_name().


% doc: Returns the manufacturer of the (supposedly connected) mobile device.
%
% For example <<"Ericsson">>, or <<"Gammu">>.
%
-spec get_device_manufacturer() -> manufacturer_name().


% doc: Returns the model of the (supposedly connected) mobile device.
%
% For example <<"E19X">>, or <<"Dummy">>.
%
-spec get_device_model() -> model_name().


% doc: Returns the firmware information from the (supposedly connected) mobile
% device.
%
% For example `{<<"11.104.20.01.00">>, <<"">>, 11.3042002}`, or `{<<"1.42.0">>,
% <<"20150101">>, 1.42}`.
%
-spec get_firmware_information() ->
    { revision_text(), date_text(), revision_number() }.


% doc: Returns the IMEI/serial number of the (supposedly connected) mobile
% device.
%
-spec get_imei_code() -> imei().


% doc: Returns the hardware information reported by the (supposedly connected)
% mobile device.
%
% For example <<"FOO DUMMY BAR">>, or
% <<"Gammu error: Function not supported by phone.">>.
%
-spec get_hardware_information() -> hardware_info().


% doc: Returns the IMSI (*International Mobile Subscriber Identity*) code,
% typically stored in the SIM card.
%
-spec get_imsi_code() -> imsi_code().


% doc: Reads the current signal quality (strength and error rate).
%
% Typical value for signal strength is -51 dBm (100%) for an actual device, -42
% dBm (42%) for an emulated one.
%
% Note that the returned error rate might be -1% (actual) or 0% (emulated).
%
-spec get_signal_quality() ->
    { signal_strength(), signal_strength_percent(), error_rate() }.





% Version-related functions.


-doc "Returns the version of the Mobile library being used.".
-spec get_mobile_version() -> three_digit_version().
get_mobile_version() ->
    basic_utils:parse_version( get_mobile_version_string() ).



-doc "Returns the version of the Mobile library being used, as a string.".
-spec get_mobile_version_string() -> ustring().
get_mobile_version_string() ->
    % As defined (uniquely) in GNUmakevars.inc:
    ?mobile_version.



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



-doc """
Tells whether Ceylan-Mobile may be used on this computer; not supposed to crash
ever.

Tells notably if the backend is available and is able to find a suitable
configuration file; for example does not tell whether an actual device can be
used (see `has_actual_device/0` for that).

Avoids the user code to have to trigger a dummy operation and catch a possible
exception in order to know whether the backend is usable.
""".
-spec is_available() -> boolean().
is_available() ->

    % No better/simpler/more reliable test known than:
    %
    % (inhibiting temporarily any trace bridge, as we do no want a spurious
    % Seaplus crash report due to Mobile unavaibility to pollute the overall
    % application traces)

    AnyPastTraceBridgeInfo = trace_bridge:inhibit_any(),

    try get_device_name() of

        DevName ->
            cond_utils:if_defined( mobile_debug_driver,
                trace_utils:debug_fmt( "Ceylan-Mobile considered to be "
                    "available (as device '~ts').", [ DevName ] ),
                basic_utils:ignore_unused( DevName ) ),
            true

    % Intercepting just the exception class and pattern of interest:
    catch

        throw:{ driver_crashed, unknown_reason } ->

            cond_utils:if_defined( mobile_debug_driver, trace_utils:debug(
                "Ceylan-Mobile considered as not available, as it "
                "cannot perform a Gammu test operation (driver crash)." ) ),

            false;

        % Never crashing:
        Class:Pattern ->
            trace_utils:error_fmt( "Unexpected exception raised "
                "when checking the availability of Gammu: "
                "exception class is ~p, pattern is ~p.", [ Class, Pattern ] ),

            false

    after

        % In all cases (no impact on returned value):
        trace_bridge:restore_any( AnyPastTraceBridgeInfo )

    end.




-doc """
Starts the Mobile service.

We define our own service-specific starting procedure, knowing that a call to
the corresponding Seaplus start will be automatically added, at first position,
by the Seaplus parse transform.
""".
-spec start() -> void().
start() ->
    start_common().



-doc "Starts and links the Mobile service.".
-spec start_link() -> void().
start_link() ->
    % (actually no process is created)
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



-doc """
Returns a set containing all characters of the GSM, 7bit alphabet that shall not
be escaped, for faster lookups.
""".
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



-doc "Returns a suitable (bijective) encoding table.".
-spec create_encoding_table() -> encoding_table().
create_encoding_table() ->
    % A build-time const_bijective_table would be overkill:
    bijective_table:new( [ { unicode_uncompressed, 1 },
                           { unicode_compressed,   2 },
                           { gsm_uncompressed,     3 },
                           { gsm_compressed,       4 },
                           { eight_bit,            5 } ] ).



-doc """
Returns hardware information regarding the (supposedly connected) mobile device.

Throws an exception on failure, typically if the operation is not supported by
the device.

We override this function to throw an exception on failure, rather than for
example sending tagged error tuples.
""".
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



-doc """
Returns the name and version of the backend used.

We override this function for convenience: the C-side just returns the Gammu
version as a string (e.g. `"1.40.0"`), we prefer return a more proper version
tuple (e.g. `{1,40,0}`).
""".
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



-doc """
Tells whether a real (non-virtual, i.e. non-emulated) device is connected.
""".
-spec has_actual_device() -> boolean().
has_actual_device() ->
    % Best criterion:
    get_device_manufacturer() =/= <<"Gammu">>.




% For the (key) sending of SMS, we override a lot the default Seaplus
% behaviours.




% Regular (mono-part) section.


-doc """
Sends the specified, regular (that is non-multipart) SMS (of class 1) to the
specified recipient, using an automatically-detected encoding.

Returns whether it succeeded, and the message TPMR reference.
""".
-spec send_regular_sms( any_sms_message(), any_mobile_number() ) ->
                                            sms_sending_report().
send_regular_sms( AnyMessage, AnyMobileNumber ) ->
    send_regular_sms( AnyMessage, AnyMobileNumber, _Class=1 ).



-doc """
Sends a regular (non-multipart) SMS to the specified recipient, using the
specified class and an automatically-detected encoding.

Returns whether it succeeded, and the message TPMR reference.
""".
-spec send_regular_sms( any_sms_message(), any_mobile_number(), sms_class() ) ->
                                            sms_sending_report().
send_regular_sms( AnyMessage, AnyMobileNumber, Class ) ->

    Message = text_utils:ensure_string( AnyMessage ),

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
            %   "Sending '~ts' as a single SMS (as requested), with "
            %   "encoding ~ts, yet expecting it to be truncated.",
            %   [ ReadyMessage, Encoding ] ),

            { Encoding, ReadyMessage }

    end,

    send_regular_sms( ActualMessage, AnyMobileNumber, Class, ActualEncoding ).



-doc """
Sends a regular (non-multipart) plain-string SMS to the specified recipient,
using the specified class and encoding.

Returns whether it succeeded, and the message TPMR reference.
""".
-spec send_regular_sms( sms_message(), any_mobile_number(), sms_class(),
                        encoding() ) -> sms_sending_report().
send_regular_sms( Message, AnyMobileNumber, Class, Encoding )
        when is_list( Message ) andalso is_integer( Class )
             andalso is_atom( Encoding ) ->

    % Only available directly in this (overridden) function:
    PortKey = seaplus:get_service_port_key(),
    FunctionDriverId = seaplus:get_function_driver_id(),

    %trace_bridge:debug_fmt( "Executing send_regular_sms "
    %    "(port key: ~ts; function driver id: ~B).",
    %    [ PortKey, FunctionDriverId ] ),

    % Exchanging binaries and directly numerical identifiers is more efficient:

    MessageBin = unicode:characters_to_binary( Message ),
    MobileNumberBin = text_utils:ensure_binary( AnyMobileNumber ),
    EncodingEnum = encoding_to_enum( Encoding ),

    Args = [ MessageBin, MobileNumberBin, Class, EncodingEnum ],

    %trace_bridge:debug_fmt( "send_regular_sms/4 sending arguments ~p.",
    %                        [ Args ] ),

    % Applies also for the regular SMS, as it is using the same callback as for
    % the multipart SMSs:
    %
    process_sending_feedback( PortKey, FunctionDriverId, Args ).



-doc """
Sends a regular (non-multipart) SMS to the specified recipients, using the
specified class and encoding.

Returns (in-order), for each sending, whether it succeeded, and the
corresponding message TPMR reference.
""".
-spec send_regular_sms_multi( any_sms_message(), [ any_mobile_number() ],
    sms_class(), encoding() ) -> [ sms_sending_report() ].
send_regular_sms_multi( AnyMessage, AnyMobileNumbers, Class, Encoding )
        % Extraneous early checkings, but more user-friendly that way:
        when is_list( AnyMobileNumbers ) andalso is_integer( Class )
             andalso is_atom( Encoding ) ->

    % Better done once for all:
    Message = text_utils:ensure_string( AnyMessage ),

    % Cannot factor much here as PortKey/FunctionDriverId auto-set by Seaplus:
    RevReports = lists:foldl(
        fun( AnyMobNumber, AccReports ) ->
            Report = send_regular_sms( Message, AnyMobNumber, Class, Encoding ),
            [ Report | AccReports ]
        end,
        _Acc0=[],
        _List=AnyMobileNumbers ),

    lists:reverse( RevReports ).




% Multipart section.

-doc """
Sends a multipart SMS, using default class 1 and an automatically-detected
encoding.

Returns whether it succeeded, and the message TPMR reference.
""".
-spec send_multipart_sms( any_sms_message(), any_mobile_number() ) ->
                                            sms_sending_report().
send_multipart_sms( AnyMessage, AnyMobileNumber ) ->
    send_multipart_sms( AnyMessage, AnyMobileNumber, _Class=1 ).



-doc """
Sends a multipart SMS, using the specified class and an automatically-detected
encoding.

Returns whether it succeeded, and the message TPMR reference.
""".
-spec send_multipart_sms( any_sms_message(), any_mobile_number(),
                          sms_class() ) -> sms_sending_report().
send_multipart_sms( AnyMessage, AnyMobileNumber, Class ) ->

    Message = text_utils:ensure_string( AnyMessage ),

    % We directly branch to the more complete version, the only one to be known
    % of the driver:
    %
    { ActualEncoding, ActualMessage } = case scan_characters( Message ) of

        { single_sms, Encoding, ReadyMessage } ->

            %trace_bridge:warning_fmt(
            %   "Sending '~ts' as a multipart SMS (as requested), with "
            %   "encoding ~ts, yet believing a single-part SMS would have "
            %   "sufficed.", [ ReadyMessage, Encoding ] ),

            { Encoding, ReadyMessage } ;


        { multiple_sms, Encoding, ReadyMessage } ->

            %trace_bridge:debug_fmt( "Sending '~ts' as a multipart SMS, with "
            %    "encoding ~ts.", [ ReadyMessage, Encoding ] ),

            { Encoding, ReadyMessage }

    end,

    send_multipart_sms( ActualMessage, AnyMobileNumber, Class, ActualEncoding ).



-doc """
Sends the specified multipart plain-string SMS to the specified recipient, using
the specified class and encoding.

Returns whether it succeeded, and the message TPMR reference.
""".
-spec send_multipart_sms( sms_message(), any_mobile_number(),
            sms_class(), encoding() ) -> sms_sending_report().
send_multipart_sms( Message, AnyMobileNumber, Class, Encoding )
        % Extraneous early checkings, but more user-friendly that way:
        when is_list( Message ) andalso is_integer( Class )
             andalso is_atom( Encoding ) ->

    % Only available directly in this (overridden) function:
    PortKey = seaplus:get_service_port_key(),
    FunctionDriverId = seaplus:get_function_driver_id(),

    cond_utils:if_defined( mobile_debug_driver, trace_bridge:debug_fmt(
        "Executing send_multipart_sms (port key: ~ts; function driver id: ~B).",
        [ PortKey, FunctionDriverId ] ) ),

    % Exchanging binaries and identifiers is more efficient:

    MessageBin = unicode:characters_to_binary( Message ),
    MobileNumberBin = text_utils:ensure_binary( AnyMobileNumber ),
    EncodingEnum = encoding_to_enum( Encoding ),

    Args = [ MessageBin, MobileNumberBin, Class, EncodingEnum ],

    cond_utils:if_defined( mobile_debug_driver, trace_bridge:debug_fmt(
        "send_multipart_sms/4 sending arguments ~p.", [ Args ] ) ),

    % Special-casing needed, since as many sending report messages will be sent
    % by the port as there will be SMS parts seen by the driver sending
    % callback, whereas we want to return exactly one message to the caller:
    %
    process_sending_feedback( PortKey, FunctionDriverId, Args ).



-doc """
Sends the specified SMS to the specified recipients, using the specified class
and encoding.

Returns (in-order), for each sending, whether it succeeded, and the
corresponding message TPMR reference.
""".
-spec send_multipart_sms_multi( sms_message(), [ any_mobile_number() ],
        sms_class(), encoding() ) -> sms_sending_report().
send_multipart_sms_multi( AnyMessage, AnyMobileNumbers, Class, Encoding )
        % Extraneous early checkings, but more user-friendly that way:
        when is_list( AnyMobileNumbers ) andalso is_integer( Class )
             andalso is_atom( Encoding ) ->

    % Better done once for all:
    Message = text_utils:ensure_string( AnyMessage ),

    % Cannot factor much here as PortKey/FunctionDriverId auto-set by Seaplus:
    RevReports = lists:foldl(
        fun( AnyMobNumber, AccReports ) ->
            Report = send_multipart_sms( Message, AnyMobNumber, Class,
                                         Encoding ),
            [ Report | AccReports ]
        end,
        _Acc0=[],
        _List=AnyMobileNumbers ),

    lists:reverse( RevReports ).



% Higher-level sendings.


-doc """
Sends the specified SMS (of class 1) to the specified recipient, determining
automatically the best encoding to use, and whether a regular SMS or a multipart
one is needed.

Returns whether this sending succeeded, and the corresponding message TPMR
reference.

This is the most advanced single-recipient SMS-sending primitive, switching
automatically to the right lower-level one, for the default class 1.
""".
-spec send_sms( any_sms_message(), any_mobile_number() ) ->
                                            sms_sending_report().
send_sms( AnyMessage, AnyMobileNumber ) ->
    send_sms( AnyMessage, AnyMobileNumber, _Class=1 ).



-doc """
Sends the specified SMS, of the specified class to the specified recipient,
determining automatically the best encoding to use, and whether a regular SMS or
a multipart one is needed.

Returns whether this sending succeeded, and the corresponding message TPMR
reference.

This is the most advanced single-recipient SMS-sending primitive, switching
automatically to the right lower-level one, based on the specified class.
""".
-spec send_sms( any_sms_message(), any_mobile_number(), sms_class() ) ->
                                            sms_sending_report().
send_sms( AnyMessage, AnyMobileNumber, Class ) ->

    Message = text_utils:ensure_string( AnyMessage ),

    % Select the right sending primitive to call:
    case scan_characters( Message ) of

        { single_sms, Encoding, ReadyMessage } ->

            %trace_bridge:debug_fmt( "Sending '~ts' as a single SMS, with "
            %   "class ~B and encoding ~ts.",
            %   [ ReadyMessage, Class, Encoding ] ),

            send_regular_sms( ReadyMessage, AnyMobileNumber, Class, Encoding );


        { multiple_sms, Encoding, ReadyMessage } ->

            %trace_bridge:debug_fmt( "Sending '~ts' as a multipart SMS, with "
            %   "class ~B and encoding ~ts.",
            %   [ ReadyMessage, Class, Encoding ] ),

            send_multipart_sms( ReadyMessage, AnyMobileNumber, Class, Encoding )

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

    % Can only be decided once all characters have been examined (as even the
    % last one may be a Unicode one):
    %
    SMSMultiplicity = case GSMUCharCount > 160 of

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



-doc """
Tells whether the specified character may be encoded in the default GSM
non-escaped alphabet.

We were initially considering to rely on a well-crafted list, however a set is
by far more appropriate here.

We used to try to favour ranges over cherry-picked codes, and roughly from the
most frequent characters to the least:
""".
%is_gsm_char( C ) when C >= $a andalso C =< $z->
%   true;
%
%is_gsm_char( C ) when C >= $0 andalso C =< $9->
%   true;
%
%is_gsm_char( C ) when C >= $A andalso C =< $Z->
%   true;
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



-doc """
Processes the (Erlang) messages that are sent back by the driver after a SMS
sending.

Should multiple elementary SMSs have to be sent:
 - if all these sendings are successful, reports `send_success` with the TPMR of
   the last one sent
 - otherwise reports `send_failure` with the TPMR of the first to fail
""".
% The sending of some SMS messages (at least the multipart ones) results in
% multiple calls of the (unique) C-level driver sending callback, and these
% calls result in turn in multiple sending report (Erlang) messages to be sent
% to the Erlang part (e.g. {send_success, _TPMR=106}, {send_success, 107},
% etc.). As even the Mobile (Erlang) code has no real way of knowing how many of
% such messages would have to be received, the callback shall return it, and
% this part shall wait for all and only return a single, overall message for the
% whole sending (otherwise it is not tractable for the user code).
%
-spec process_sending_feedback( service_key(), function_driver_id(),
                                function_params() ) -> sms_sending_report().
process_sending_feedback( ServiceKey, FunctionDriverId, Args ) ->

    % Not just a sms_sending_report() anymore, we need to know how many of them
    % shall be expected:
    %
    { SMSSendingStatus, Tpmr, ReportCount } =
        seaplus:call_port_for( ServiceKey, FunctionDriverId, Args ),

    cond_utils:if_defined( mobile_debug_driver, trace_bridge:debug_fmt(
        "Expecting ~B sending feedback messages, "
        "starting with TPMR ~B.", [ ReportCount, Tpmr ] ) ),

    % Not expected to have vanished after the previous call:
    TargetPort = process_dictionary:get( ServiceKey ),

    % Note that we used to match incrementing TPMRs, yet the Gammu dummy device
    % does not manage them properly (they are always equal to 255); so now we
    % just count messages.
    %
    aggregate_sending_feedbacks( SMSSendingStatus, _Current=Tpmr,
                                 % _Max=Tpmr+ReportCount-1, TargetPort ).
                                 _RemainingSMSCount=ReportCount-1, TargetPort ).



% (helper)
%

% Finished here:
%aggregate_sending_feedbacks( SMSSendingStatus, _CurrentTpmr=MaxTpmr, MaxTpmr,
%                             _TargetPort ) ->
aggregate_sending_feedbacks( SMSSendingStatus, CurrentTpmr,
                             _RemainingSMSCount=0, _TargetPort ) ->

    cond_utils:if_defined( mobile_debug_driver, trace_bridge:debug_fmt(
        "(all sending feedback aggregated, with TPMR ~B)", [ CurrentTpmr ] ) ),

    { SMSSendingStatus, CurrentTpmr };

% At least one more:
%aggregate_sending_feedbacks( SMSSendingStatus, CurrentTpmr, MaxTpmr,
aggregate_sending_feedbacks( SMSSendingStatus, CurrentTpmr, RemainingSMSCount,
                             TargetPort ) ->

    % Mostly the same as in seaplus:call_port_for/3:
    Res = receive

        { TargetPort, { data, BinAnswer } } ->
            try
                binary_to_term( BinAnswer )
            catch

                _:E ->
                    throw( { sms_sending_deserialisation_failed, E } )

            end

    end,

    cond_utils:if_defined( mobile_debug_driver, trace_bridge:debug_fmt(
        "Aggregating sending feedback ~p.", [ Res ] ) ),

    case Res of

        % Expected to be CurrentTpmr:
        { send_success, RecvTpmr, _SameReportCount } ->
            % Total reported to check it is constant for this series:
            %trace_bridge:debug_fmt( "Got (success) TPMR ~B (total: ~B).",
            %                        [ RecvTpmr, SameReportCount ] ),

            NewTpmr = case SMSSendingStatus of

                % Keeping the one of the first failure:
                send_failure ->
                     CurrentTpmr;

                % This one is the last success:
                send_success ->
                    RecvTpmr

            end,
            % Keeping the same status:
            aggregate_sending_feedbacks( SMSSendingStatus, NewTpmr,
                                         RemainingSMSCount-1, TargetPort );

        { send_failure, RecvTpmr, _SameReportCount } ->
            %trace_bridge:debug_fmt( "Got (failure) TPMR ~B (total: ~B).",
            %                        [ RecvTpmr, SameReportCount ] ),
            NewTpmr = case SMSSendingStatus of

                % Keeping the one of the first failure:
                send_failure ->
                     CurrentTpmr;

                % This one is the first failure:
                send_success ->
                     RecvTpmr

            end,
            aggregate_sending_feedbacks( send_failure, NewTpmr,
                                         RemainingSMSCount-1, TargetPort );

        Other ->
            throw( { sms_sending_unexpected_message, Other } )

    end.



-doc """
Sends the specified SMS (of class 1) to the specified recipients, determining
automatically the best encoding to use, and whether a regular SMS or a multipart
one is needed.

Returns whether these sendings succeeded, and the corresponding message TPMR
references.

This is the most advanced multi-recipient SMS-sending primitive, switching
automatically to the right lower-level one, for the default class 1.
""".
-spec send_sms_multi( sms_message(), [ any_mobile_number() ] ) ->
                                            [ sms_sending_report() ].
send_sms_multi( Message, AnyMobileNumbers ) ->
    send_sms_multi( Message, AnyMobileNumbers, _Class=1 ).



-doc """
Sends the specified SMS, of the specified class to the specified recipients,
determining automatically the best encoding to use, and whether a regular SMS or
a multipart one is needed.

Returns whether these sendings succeeded, and the corresponding message TPMR
references.

This is the most advanced multi-recipient SMS-sending primitive, switching
automatically to the right lower-level one, based on the specified class.
""".
-spec send_sms_multi( any_sms_message(), [ any_mobile_number() ],
                      sms_class() ) -> [ sms_sending_report() ].
send_sms_multi( AnyMessage, AnyMobileNumber, Class ) ->

    Message = text_utils:ensure_string( AnyMessage ),

    % Select the right sending primitive to call:
    case scan_characters( Message ) of

        { single_sms, Encoding, ReadyMessage } ->

            %trace_bridge:debug_fmt( "Sending '~ts' as a single SMS, with "
            %   "class ~B and encoding ~ts.",
            %   [ ReadyMessage, Class, Encoding ] ),

            send_regular_sms_multi( ReadyMessage, AnyMobileNumber, Class,
                                    Encoding );


        { multiple_sms, Encoding, ReadyMessage } ->

            %trace_bridge:debug_fmt( "Sending '~ts' as a multipart SMS, with "
            %   "class ~B and encoding ~ts.",
            %   [ ReadyMessage, Class, Encoding ] ),

            send_multipart_sms_multi( ReadyMessage, AnyMobileNumber, Class,
                                      Encoding )

    end.



-doc """
Tells whether, among the specified reports, failed sendings are found: returns a
(possibly empty) list of the mobile numbers (in the same form as specified; in
reverse order) to which the corresponding sending failed.
""".
-spec detect_failed_sendings( [ sms_sending_report() ],
        [ any_mobile_number() ] ) -> [ any_mobile_number() ].
detect_failed_sendings( Reports, Numbers ) ->
    detect_failed_sendings( Reports, Numbers, _AccNums=[] ).


% (helper)
detect_failed_sendings( _Reports=[], _Numbers=[], AccNum ) ->
    % Order does not matter:
    AccNum;

detect_failed_sendings( _Reports=[], Numbers, _AccNum ) ->
    throw( { unexpected_mobile_numbers, Numbers } );

detect_failed_sendings( Reports, _Numbers=[], _AccNum ) ->
    throw( { unexpected_sending_reports, Reports } );

detect_failed_sendings( _Reports=[ { send_success, _Tpmr } | TRep ],
                        _Numbers=[ _AnyNum | TNum ], AccNum ) ->
    detect_failed_sendings( TRep, TNum, AccNum );

detect_failed_sendings( _Reports=[ { send_failure, _Tpmr } | TRep ],
                        _Numbers=[ AnyNum | TNum ], AccNum ) ->
    NewAccNum= [ AnyNum | AccNum ],
    detect_failed_sendings( TRep, TNum, NewAccNum );

detect_failed_sendings( _Reports=[ R | _TRep ], _Numbers, _AccNum ) ->
    throw( { invalid_sending_report, R } ).




-doc "Helper; see the enum encoding in the corresponding driver.".
-spec encoding_to_enum( encoding() ) -> encoding_enum().
encoding_to_enum( Encoding ) ->
    EncTable = process_dictionary:get( ?mobile_encoding_key ),
    bijective_table:get_second_for( Encoding, EncTable ).


-doc "Reverse conversion.".
-spec enum_to_encoding( encoding_enum() ) -> encoding().
enum_to_encoding( EncodingEnum ) ->
    EncTable = process_dictionary:get( ?mobile_encoding_key ),
    bijective_table:get_first_for( EncodingEnum, EncTable ).



-doc """
Reads all SMS already received (if any), and, if `DeleteOnReading` is true is
specified, deletes them as soon as they are read.

Does not block.

Specialised here to transform conveniently its outputs.
""".
-spec read_all_sms( boolean() ) -> [ received_sms() ].
read_all_sms( DeleteOnReading ) ->

    % These two pseudo-calls are replaced at compilation time by the Seaplus
    % parse transform with the relevant immediate values:

    PortKey = seaplus:get_service_port_key(),
    FunctionDriverId = seaplus:get_function_driver_id(),

    %trace_bridge:debug_fmt( "Executing read_all_sms "
    %    "(port key: ~ts; function driver id: ~B).",
    %    [ PortKey, FunctionDriverId ] ),

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



-doc """
Converts a transmitted subset of `GSM_SMSMessage` into a `received_sms` record.
""".
to_sms( { BinSenderNumber, EncodingValue, MessageReference, Timestamp,
          BinText } ) ->

    %trace_utils:debug_fmt( "BinText = '~ts', i.e.: ~w.",
    %                       [ BinText, BinText ] ),

    #received_sms{ sender_number=BinSenderNumber,
                   encoding=enum_to_encoding( EncodingValue ),
                   text=BinText,
                   message_reference=MessageReference,
                   timestamp=Timestamp }.



-doc """
Returns an overall, textual information about the current mobile setting.

(Ceylan-Mobile must have already been started)
""".
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



-doc "Returns a textual description of the specified received SMS.".
received_sms_to_string( #received_sms{ sender_number=BinSenderNumber,
                                       encoding=Encoding,
                                       text=BinText,
                                       message_reference=MsgRef,
                                       timestamp=Timestamp } ) ->
    text_utils:format( "received SMS sent from number '~ts' (with encoding ~ts)"
        " whose text is: '~ts' (reference: ~p, sending timestamp: ~ts)",
        [ BinSenderNumber, Encoding, BinText, MsgRef,
          time_utils:timestamp_to_string( Timestamp ) ] ).



-doc """
Stops the Mobile service.

Service-specific stop procedure.
""".
-spec stop() -> void().
stop() ->

    cond_utils:if_defined( mobile_debug_base,
                           trace_bridge:debug( "Stopping Mobile." ) ),

    [ process_dictionary:remove_existing( K )
        || K <- [ ?mobile_gsm_charset_key, ?mobile_encoding_key ] ].
