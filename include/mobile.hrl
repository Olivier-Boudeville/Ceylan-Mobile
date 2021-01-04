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



% Stores information about a received SMS.
-record( received_sms, {

	% The mobile number of the SMS sender (as a binary):
	sender_number :: bin_mobile_number(),

	% The encoding used for the transmitted text:
	encoding :: encoding(),

	% The corresponding text:
	text :: bin_sms_message(),

	% The GSM network message reference:
	message_reference :: sms_tpmr(),

	% The best sending timestamp determined for this message:
	timestamp :: sms_timestamp() } ).
