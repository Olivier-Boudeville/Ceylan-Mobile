% Description of the Mobile OTP library application, typically used by rebar3.

% Note: if this file is named mobile.app, it is a *generated* file, whose real
% source is conf/mobile.app.src, from which _build/lib/mobile/ebin/mobile.app is
% obtained and copied to ebin/mobile.app; finally src/mobile.app.src is a mere
% symlink to this last file, so we have:
%
% ./conf/mobile.app.src [only real source]
% ./_build/lib/mobile/ebin/mobile.app
% ./ebin/mobile.app
% ./src/mobile.app.src -> ../ebin/mobile.app
%
% For more information see the Ceylan-Myriad 'rebar3-create-app-file' make
% target and its associated comments.


% See also:
% - http://erlang.org/doc/man/app.html
% - https://learnyousomeerlang.com/building-otp-applications


{application, mobile,
 [{description, "Ceylan-Mobile, controlling mobile phones and 3G keys from Erlang, as an OTP library application here (see http://mobile.esperide.org)"},
  {vsn, "1.0.5"},

  % No process registered:
  {registered, []},

  % Regarding Myriad, see http://myriad.esperide.org/myriad.html#otp:
  {applications, [kernel, stdlib, myriad, seaplus]},

  %{env,[]},

  % Flat hierarchy in ebin here:
  {modules, [mobile]},

  {licenses, ["Ceylan-Mobile is licensed by its author (Olivier Boudeville) under a disjunctive tri-license, giving you the choice of one of the three following sets of free software/open source licensing terms:
	- the Mozilla Public License (MPL), version 1.1 or later (very close to the former Erlang Public License, except aspects regarding Ericsson and/or the Swedish law)
	- the GNU General Public License (GPL), version 3.0 or later
	- the GNU Lesser General Public License (LGPL), version 3.0 or later"]},

  % Library application, not an active one, so no specific behaviour of its own:
  % {mod, {mobile_app,[]}}

  { links, [ {"Official website", "http://mobile.esperide.org" },
			 {"Github", "https://github.com/Olivier-Boudeville/Ceylan-Mobile"} ]}

  %{exclude_files, []}

 ]}.
