
.. _Top:


.. title:: Welcome to the Ceylan-Mobile 1.0 documentation

.. comment stylesheet specified through GNUmakefile


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex

.. comment Would appear too late, can only be an be used only in preamble:
.. comment :raw-latex:`\usepackage{graphicx}`
.. comment As a result, in this document at least a '.. figure:: XXXX' must
.. exist, otherwise: 'Undefined control sequence \includegraphics.'.


:raw-html:`<a name="mobile_top"></a>`

:raw-html:`<div class="banner"><p><em>Mobile 1.0 documentation</em> <a href="http://mobile.esperide.org">browse latest</a> <a href="https://olivier-boudeville.github.io/Ceylan-Mobile/mobile-1.0.html">browse mirror</a> <a href="mobile-1.0.pdf">get PDF</a> <a href="#mobile_top">go to top</a> <a href="#mobile_bottom">go to bottom</a> <a href="mailto:about(dash)mobile(at)esperide(dot)com?subject=[Ceylan-Mobile 1.0]%20Remark">email us</a></p></div>`



:raw-html:`<center><img src="mobile-title.png" width="50%"></img></center>`
:raw-latex:`\includegraphics[scale=0.70]{mobile-title.png}`




---------------------------------------------------------
Mobile: Controlling Mobile Phones and 3G Keys from Erlang
---------------------------------------------------------


:Organisation: Copyright (C) 2019-2019 Olivier Boudeville
:Contact: about (dash) mobile (at) esperide (dot) com
:Creation date: Sunday, March 3, 2019
:Lastly updated: Sunday, March 24, 2019
:Dedication: Users and maintainers of the ``Mobile`` library, version 1.0.
:Abstract:

	The role of the ``Mobile`` library is to control mobile phones from Erlang.
	A typical use case is to send SMS from Erlang thanks to a connected 3G USB key.


.. meta::
   :keywords: Mobile, cellular, phone, 3G, SMS, MMS, Erlang


The latest version of this documentation is to be found at the `official Mobile website <http://mobile.esperide.org>`_ (``http://mobile.esperide.org``).

:raw-html:`This Mobile documentation is also available in the PDF format (see <a href="mobile-1.0.pdf">mobile.pdf</a>), and mirrored <a href="http://olivier-boudeville.github.io/Ceylan-Mobile/mobile-1.0.html">here</a>.`

:raw-latex:`The documentation is also mirrored \href{https://olivier-boudeville.github.io/Ceylan-Mobile/mobile-1.0.html}{here}.`



:raw-latex:`\pagebreak`



.. _`table of contents`:


.. contents:: Table of Contents
  :depth: 3


:raw-latex:`\pagebreak`


Overview
========

A typical use-case is when one wants to **send SMS from a gateway** (server), for example in order to perform home automation, possibly together with all kinds of fancy personal services (event reminder, UPS notifications, etc.).

For that, in addition to Ceylan-Mobile and its software dependencies, one would need here:

- an **USB-connected 3G device** (typically an older, dedicated phone or, more preferably, a suitable key)
- a **SIM card** enabling the use of the 3G network of a telecom carrier of one's choice
- a proper **operating-system support** thereof (a GNU/Linux box, most probably)

All these points are detailed below.


An alternate, less satisfying solution [#]_ could be to use the services of a SMS provider through a web gateway; one may look at `sms_utils.erl <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/utils/sms_utils.erl>`_ for a basic example of it.


.. [#] Not nearly as fun: more constraints (ex: credit expiration), probably more expensive, and of course the SMS sending system cannot be used to notify for example that your Internet connection was lost, possibly cut off by burglars...


The solution presented here relies on the `Gammu <https://wammu.eu/gammu/>`_ library for the actual control of the 3G device, and on `Ceylan-Seaplus <http://seaplus.esperide.org/>`_ to make it available to Erlang.




Securing the Prerequisites
==========================

Please read the full document prior to operating, as iterations, trials and errors will probably have to be performed before, hopefully, succeeding ultimately.



Carrier Subscription: Getting a relevant SIM Card first
-------------------------------------------------------

A 3G device without a companion SIM card would not be of much use.

Finding a good mobile package is quite country-specific. For example French customers might enjoy rather inexpensive, if not free, `options <http://mobile.free.fr/fiche-forfait-2-euros.html>`_.

As we want an automated use of this SIM card (through the 3G device selected in the next section), one should ensure that, in the card's configuration, the request for the PIN code has been disabled.

This can be done by inserting the SIM card in a mobile phone, and through the settings disabling once for all said verification. This may also be a good way of checking whether the SIM card works properly before hacking around.



Relying on a Suitable USB Mobile Phone or 3G Key
------------------------------------------------

Basically, one just has to insert the SIM card, connect the device to the computer and start sending SMS to friends.

Well, no. Welcome to a surprising mess instead!

First of all, as we understand it, mobiles are far less appropriate than 3G keys for this exercice (they are not well supported, they tend to enter various sleep modes), but be reassured that your mileage may vary with keys as well.

In order to select a proper device (phone or key), first of all one should look at the `Gammu Phone Database <https://wammu.eu/phones/>`_ (which lists keys as well).

We tried to favour the *acknowledged* entries in this database, moreover the ones with multiple success reports.

For the record, we selected only Huawei chips (supposedly becoming, for better or for worse, a de facto standard) of various quite widespread offers, and bought 4 secondhand 3G keys of different models [#]_, which we named that way:

 - ``K3G-1``: a black and orange generic model (no specific brand apparently), labelled HSDPA, with a Huawei E169 chip
 - ``K3G-2`` : white (with a green LED, invisible unless lit), from a former operator, based on a Huawei E170
 - ``K3G-3`` : white, from another former operator, based on a Huawei E172 (labelled as E1752); can host an additional MicroSD card
 - ``K3G-4`` : white, from same former operator as ``K3G-2``, based on Huawei E180 (firmware ``11.104.16.01.00``), with a rotating USB connection; can also host an additional MicroSD card

.. [#] For a whopping expensively bill of 10 euros.

To anticipate a bit:

- we have been able to make good use of ``K3G-2`` (which became our "reference" key, used operationally) and ``K3G-4`` (kept as a spare key, should the previous one fail)
- we have not been able to durably use ``K3G-1`` and ``K3G-4`` (notably: they were regularly not responding anymore after a few, normal interactions)

So, from then on, we will mostly consider here the use of ``K3G-2``.

Finally, one should ensure that one's 3G device is not locked to a specific network or carrier. In most cases the device will have to be unlocked, so that it can accept SIM cards issued by any operator (and not just the one devices are generally bundled with).

We were told that our 4 keys were unlocked, yet none of them seemed to properly work on Linux (blocking at various steps, like when fetching their IMEI) until we tried to install them on a Windows box and also to unlock them.

Not sure which operation unblocked them, as most of the attempted operations reportedly failed or could not be properly interpreted in terms of result. This `online calculator <http://www.deblokgsm.com/server/huaweicalc-us.php>`_ for Huawei chips seemed to work (giving a NCK unlocking code and another one for the flash operation), even if it is difficult to assess whether the use of any actual code really succeeded.

We are not so keen on installing third-party, untrusted software on said Windows box (even installing the driver located on their ROM appearing as a mass storage is somewhat unpleasant), but flashing tools are required whenever having to unlock.

For them, `Sandboxie <https://www.sandboxie.com/>`_ or similar may be used in order to isolate, at least to some extent, the various software that one may try in one's quest for a correctly-behaving 3G key. At least for us, quite frequently Windows was not even able to detect that the key was inserted.

More generally, `various problems <https://stackoverflow.com/questions/29365148/gammu-stops-receiving-sms-aftar-a-while>`_ might explain why a 3G key is misbehaving (i.e. does not seem able to operate, at least `not durably <https://wiki.archlinux.org/index.php/USB_3G_Modem#Connection_halts_after_few_minutes_running>`_), including failed unlocking, buggy firmware versions and alike, and other issues discussed at later steps.

Good luck to you!



USB Supply & Connection
-----------------------

Once one managed to correctly put the SIM card in the right format (normal, micro or nano) in said device, a proper USB cable shall be used to interlink, say, the residential server and the 3G phone, whereas a 3G key could be directly connected to a computer port.

A problem might be that the device could end up being insufficiently powered device. Some people use a separately-powered USB hub, to compensate for computer USB ports that would not be powerful enough. We never experienced that problem, though.




Operating-System Support & Configuration
----------------------------------------

That's the main part. We prefer using GNU/Linux, typically `Arch Linux <https://www.archlinux.org/>`_, taken consistently as a reference here.


Kernel Modules
..............

As always, maybe new kernel modules will have to be dynamically loaded; so, should the kernel have been updated since last boot, reboot first, otherwise the (newer) modules will not match the currently running, older kernel.


Before first inserting a 3G device, we advise to record the already loaded kernel modules, in order to detect the additional ones that are needed by said device.

For example, as root:

.. code:: bash

  $ lsmod > ~/lsmod-before.txt
  # Connect 3G device and wait a bit (ex: LED blinking)
  $ lsmod > ~/lsmod-after.txt
  $ diff ~/lsmod-before.txt ~/lsmod-after.txt

In our case, the ``option`` and ``usb_wwan`` modules were loaded, so we ensured that, from now then, they were automatically loaded at boot (to avoid that a later kernel update block their loading), by creating a ``/etc/modules-load.d/for-3g-keys.conf`` file with following content::

 option
 usb_wwan



USB Identifiers & Mode Switches
...............................

The connected key will then appear as a USB device, with a USB ID in the form of ``vendor_id:product_id``, which can be for example be obtained thanks to ``lsusb``.

For example, at connection, our ``K3G-1`` key will appear as::

  Bus 003 Device 096: ID 12d1:141b Huawei Technologies Co., Ltd.


Unfortunately, this does not correspond to a (3G) modem, but to a mass storage: most keys will be detected as such (ex: as CD-ROM players), as they comprise a built-in ROM (if not an additional MicroSD slot) where typically the vendor (Windows) drivers are located. These drivers, once installed, will switch the operating mode of their key, from mass storage to modem.

Here such drivers are of no use, and what we want is to switch the keys to modems.

For that, as root, following package shall be installed first::

 $ pacman -Sy usb_modeswitch


It should install a udev rule file (``/usr/lib/udev/rules.d/40-usb_modeswitch.rules``) suitable for most 3G devices (otherwise you will have to enrich it).

Then the key should be plugged again; the vendor identifier is not expected to change, but the product identifier should, so that the key is now considered as a modem. ``journatctl -xe`` should allow to check.


For example, once connected, our ``K3G-1`` key is to spontaneously switch (almost immediately) from the previous::

  Bus 003 Device 096: ID 12d1:141b Huawei Technologies Co., Ltd.

to a newer::

  Bus 003 Device 060: ID 12d1:1446 Huawei Technologies Co., Ltd. HSPA modem


Bye bye mass storage, hello modem!


This mode switch can also be done manually, like in::

  $ sudo usb_modeswitch --verbose -J -v 0x12d1 -p 0x1446

``lsusb`` would then ultimately report, for ``K3G-2``::

		 Bus 002 Device 003: ID 12d1:1003 Huawei Technologies Co., Ltd. E220 HSDPA Modem / E230/E270/E870 HSDPA/HSUPA Modem



Managing /dev/ttyUSB* entries
.............................


Should the relevant kernel modules be available, at least one entry shall appear as ``/dev/ttyUSB*`` when a USB 3G device is connected and correctly recognised by the system.

For example, ``/dev/ttyUSB0``, ``/dev/ttyUSB1``, ``/dev/ttyUSB2`` and ``/dev/ttyUSB3`` may appear, sometimes only after a few seconds. Only a subset of them will be useable.

A tests with Gammu will tell them apart.

First, that tool shall be installed.

One's distribution should provide it, as it is fairly standard::

  $ pacman gammu


It should notably provide the Gammu library (ex: in ``/usr/lib64/libGammu.so.8.1.40.0``) and the various Gammu header (ex: the ``/usr/include/gammu/gammu/gammu*.h``).

With this package comes the ``/usr/bin/gammu`` executable (of course relying on said library), which is useful to test one's configuration.

The executable may read its test configuration from ``/etc/gammurc``, whose content may be, for example in order to test whether ``/dev/ttyUSB1`` (the tty we use for ``K3G-2``) is relevant::

 [gammu]
 device = /dev/ttyUSB1
 connection = at
 logfile = /var/log/gammu-ceylan.log
 logformat = textalldate



To check whether one's 3G device is supported by the system, one may use::

  $ gammu --identify

Note that each operation is bound to last for a few (around 3-4) seconds before returning.

Hopefully one will not end up with following information returned::

  Can not access SIM card.

or even worse::

  No response in specified timeout. Probably phone not connected.


but, after maybe some trials and errors (start by testing various ``/dev/ttyUSB*`` devices and ``connection`` settings), with something like (IDs edited for obvious reasons)::

 Device               : /dev/ttyUSB1
 Manufacturer         : Huawei
 Model                : E17X (E17X)
 Firmware             : 11.304.20.01.00
 IMEI                 : XXXXXXXXXXXXXXX
 SIM IMSI             : XXXXXXXXXXXXXXX


Congratulations, the operating system supports, at least to some extent, your device!



A problem will be afterwards that the numbers involved in the tty pseudofiles are bound to change - based on, notably, the use of the other USB ports.

So a better approach will be to use ``udev`` in order to give them a stable name, such as ``/dev/ttyUSB-my-3G-key``, thanks to a rule typically written in ``/etc/udev/rules.d/98-usb-my-3G-key.rules``, whose content would be::

  SUBSYSTEM=="tty", ATTRS{idVendor}=="12d1", ATTRS{idProduct}=="1446", \
	 SYMLINK+="ttyUSB-my-3G-key"


Then one should run::

   $ udevadm control --reload-rules && udevadm trigger

One the key is inserted again, it should be available with its new, stable name.

It can be checked more in-depth::

 $ udevadm info --query=all --name=ttyUSB-my-3G-key
  P: /devices/pci0000:00/0000:00:14.0/usb2/2-3/2-3:1.1/ttyUSB1/tty/ttyUSB1
  N: ttyUSB1
  L: 0
  S: ttyUSB-my-3G-key
  S: serial/by-path/pci-0000:00:14.0-usb-0:3:1.1-port0
  S: serial/by-id/usb-HUAWEI_Technologies_HUAWEI_Mobile-if01-port0
  [...]

One may ensure thanks to ``fuser`` that no component (Network Manager or alike) took control of it::

  $ fuser -va /dev/ttyUSB-my-3G-key
					 USER        PID ACCESS COMMAND
  /dev/ttyUSB1:

(as intended, no controller process in the way here)



To interact with such a ``/dev/ttyUSB*`` file, the user (let's name him ``sheldon``) must be in the ``uucp`` group; so, as root::

  $ gpasswd -a sheldon uucp

And, as ``sheldon``::

  $ newgrp uucp



Wrapping-up Telecom Configuration
.................................

Now, with that user, is time for a bit of configuration before testing.

One may use::

 $ gammu getsecuritystatus

to ensure that no PIN code is required before using the 3G device (hence expecting as answer: ``Nothing to enter.``).


Various calls can be made in order to convince oneself that the key operate properly::

 $ gammu battery
 Battery level        : 0 percent
 Charge state         : battery connected and is being charged

 $ gammu getallsms
 0 SMS parts in 0 SMS sequences


The `SMSC <https://en.wikipedia.org/wiki/Short_Message_service_center>`_ number of the carrier having issued one's SIM card must be set before any actual SMS sending::

  $ gammu setsmsc 1 "+33695000XYZ"

Should this operation fail, it may be a sign that the 3G device is still locked.


This can be checked::

  $ gammu getsmsc
  Location             : 1
  Number               : "+33695000XYZ"
  Default number       : ""
  Format               : Text
  Validity             : Maximum time



Then a SMS can be sent, assuming ``TARGET_NUMBER`` has been set to some sensible number (like one's mobile)::

  $ gammu sendsms TEXT ${TARGET_NUMBER} -text "Hello world!"
  If you want break, press Ctrl+C...
  Sending SMS 1/1....waiting for network answer..OK, message reference=50

As the SMSC has just been set previously, one should not get ``Failed to get SMSC number from phone``.

This can be monitored::

  $ gammu monitor 1
  Press Ctrl+C to break...
  Entering monitor mode...

  Enabling info about incoming SMS    : No error.
  SMS message received
  [...]
  We already have one pending, ignoring this one!
  SMS message received
  Enabling info about incoming CB     : Security error. Maybe no PIN?
  Enabling info about calls           : No error.
  Enabling info about USSD            : No error.
  SIM phonebook        :   0 used, 250 free
  Dialled numbers      :  10 used,   0 free
  Received numbers     :   0 used,  10 free
  Missed numbers       :   0 used,  10 free
  Own numbers          :   1 used,   4 free
  Phone phonebook      :   0 used, 100 free
  Battery level        : 0 percent
  Charge state         : battery connected and is being charged
  Signal strength      : -51 dBm
  Network level        : 100 percent
  SIM SMS status       : 9 used, 0 unread, 50 locations
  Phone SMS status     : 0 used, 0 unread, 255 locations
  Network state        : home network
  Network              : 208 15 (XXX Mobile, France), LAC F8F, CID XYZ
  Packet network state : home network
  Packet network       : 208 15 (XXX Mobile, France), LAC F8F, CID UVW
  GPRS                 : attached
  Location 4, folder "Inbox", SIM memory, Inbox folder
  SMS message
  SMSC number          : "+33695000XYZ"
  Sent                 : Sat Dec 22 21:22:14 2018 +0100
  Coding               : Default GSM alphabet (no compression)
  Remote number        : "+XXXXXXX"
  Status               : UnRead
  [...]
  Leaving monitor mode...


Once successful, one will be able to send SMS back and forth between the 3G device and "normal" phones::

 $ gammu getallsms


With this first support, one will be able to fight encodings (ex: for special characters), SMS parts (ex: for messages too large for a single SMS) and sequences. MMS should provide a lot of fun too.


Software
--------

Ceylan-Mobile relies on `Ceylan-Seaplus <https://github.com/Olivier-Boudeville/Ceylan-Seaplus>`_, which itself relies on `Ceylan-Myriad <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_.

All three of them rely on `Erlang <http://erlang.org>`_ (for the user API) and on C (for the library driver), which must therefore be both available.



Erlang Environment
..................

`Erlang <http://www.erlang.org/>`_, version 21.0 or higher, is needed.

One may obtain it from many ways, including one's distribution (ex: ``pacman erlang``), directly `from its sources <http://www.erlang.org/downloads>`_ or possibly thanks to our `install-erlang.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/install-erlang.sh>`_ script; a simple use of it is::

  $ ./install-erlang.sh --doc-install --generate-plt


One may execute ``./install-erlang.sh --help`` for more details about how to configure it.


C Environment
.............

One may use a recent enough version of GCC (ex: ``pacman gcc``).


Gammu Conventions
.................

The Gammu configuration file will be searched, on POSIX systems, first as ``~/.gammurc``, then as ``/etc/gammurc``.

For debugging purposes, using the ``dummy`` driver is quite convenient.

So for example one could have following content for ``/etc/gammurc``::

 [gammu]
 model = dummy
 connection = none
 device = /tmp/gammu-dummy-device


Create that directory (as the user to make use of Gammu) first::

 $ mkdir /tmp/gammu-dummy-device

Otherwise you get: ``you don't have the required permission.``.

It will populate this directory with data faking a real phone::

 /tmp/gammu-dummy-device
 ├── alarm
 ├── calendar
 ├── fs
 │   └── incoming
 ├── note
 ├── operations.log
 ├── pbk
 │   ├── DC
 │   ├── MC
 │   ├── ME
 │   ├── RC
 │   └── SM
 ├── sms
 │   ├── 1
 │   ├── 2
 │   ├── 3
 │   ├── 4
 │   └── 5
 └── todo



Myriad, Seaplus and Mobile
..........................

Once proper Erlang and C environments are available, the `Ceylan-Myriad repository <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_ should be cloned and built, before doing the same with the `Ceylan-Seaplus repository <https://github.com/Olivier-Boudeville/Ceylan-Seaplus>`_ and then this `Ceylan-Mobile repository <https://github.com/Olivier-Boudeville/Ceylan-Mobile>`_, like in:

.. code:: bash

 $ git clone https://github.com/Olivier-Boudeville/Ceylan-Myriad
 $ cd Ceylan-Myriad && make all && cd ..
 $ git clone https://github.com/Olivier-Boudeville/Ceylan-Seaplus
 $ cd Ceylan-Seaplus && make all && cd ..
 $ git clone https://github.com/Olivier-Boudeville/Ceylan-Mobile
 $ cd Ceylan-Mobile && make all


Then only one will be able to fight encodings (ex: for special characters) and SMS parts and sequences. MMS should provide a lot of fun too.


Testing Ceylan-Mobile
.....................

To test the current functional coverage, one may run `mobile_test.erl <https://github.com/Olivier-Boudeville/Ceylan-Mobile/blob/master/src/mobile_test.erl>`_; from the root of the ``Ceylan-Mobile`` clone (once built, and assuming here using the ``dummy`` Gammu driver - so that the test can be run even if having no 3G device)::

 $ cd src
 $ make mobile-test
		Running unitary test mobile_run (second form) from mobile_test mobile.beam
  --> Testing module mobile_test.

  Testing the Ceylan-Mobile service.
  Back-end information: {gammu,{1,40,0}}.
  Device manufacturer: Gammu.
  Device model: Dummy.
  Firmware information: revision is '1.40.0', date is '20150101' and revision number is 1.4.
  IMEI code: '999999999999999'.
  Hardware information: 'FOO DUMMY BAR'.
  IMSI code: '994299429942994'.
  Signal quality: signal strength is 42 dBm (42%), error rate is 0%.
  [...]

One may also have a look at the resulting Seaplus log  (ex: ``seaplus-driver.27168.log``; timestamps removed for terseness)::

  [debug] Starting Seaplus session...
  [debug] Starting the Seaplus C driver, with a buffer of 32768 bytes.
  [trace] At start-up: currently allocated blocks: 0; length of freelist: 0.
  [trace] Driver started.
  [debug] Starting Gammu.
  [debug] Executing get_backend_information/0.
  [debug] Executing get_device_manufacturer/0.
  [debug] Executing get_device_model/0.
  [debug] Executing get_firmware_information/0.
  [debug] Executing get_imei_code/0.
  [debug] Executing get_hardware_information/0.
  [debug] Executing get_imsi_code/0.
  [debug] Executing get_signal_quality/0.
  [...]
  [debug] Stopping Gammu.
  [debug] Stopping the Seaplus C driver.
  [trace] At stop: currently allocated blocks: 0; length of freelist: 0.
  [debug] Stopping Seaplus session.

:raw-latex:`\pagebreak`



Issues & Planned Enhancements
=============================

The coverage of the Gammu APIs could be increased (not specifically tricky, just time-consuming).



Support
=======

Bugs, questions, remarks, patches, requests for enhancements, etc. are to be sent to the `project interface <https://github.com/Olivier-Boudeville/Ceylan-Mobile>`_, or directly at the email address mentioned at the beginning of this document.




Ceylan-Mobile Inner Workings
============================

Mobile relies on:

- `libGammu <https://wammu.eu/libgammu/>`_ (GPLv2), for the actual mobile phone support
- `Ceylan-Seaplus <https://github.com/Olivier-Boudeville/Ceylan-Seaplus>`_ (LGPLv3), for the integration of the previous library to Erlang

Ceylan-Mobile links directly to (lower-level) Gammu library services, instead of using the Gammu SMSD daemon, as it provides similar features, such as driving the mobile-side operations and polling it for incoming events.

Ceylan-Mobile respects the way Gammu searches for, and reads, its configuration file (no change needed, the same configuration can be used on the command-line and with Ceylan-Mobile).

Internally, Gammu uses state machines.

Not specifically used/supported: WAP, FM stations, GPRS access points, MMS, SyncML, phonebooks, calendars, alarms, TO-DO lists, notes, profiles, chats, voice mailboxes, vCards, security (PIN, PIN2, PUK, PUK2), ringtones, JAD files, voice call management, cell broadcast, USSD, callbacks, backups, etc.

Please feel free to enrich Ceylan-Mobile!

A source of inspiration has been `python-gammu <https://github.com/gammu/python-gammu/>`_.



A Few Information Pointers
==========================

About 3G Devices
----------------

- `USB 3G Modem <https://wiki.archlinux.org/index.php/USB_3G_Modem>`_, by Arch Linux
- `USB_ModeSwitch <https://wiki.gentoo.org/wiki/USB_ModeSwitch>`_, by Gentoo Linux
- `About Huawei E173D <https://metodiew.com/huawei-e173s-and-ubuntu/>`_ (and Linux)
- in French: `with a Raspberry Pi <http://blogmotion.fr/diy/tutoriel-gammu-cle-3g-dongle-16409>`_



About Gammu
-----------

- `libGammu C API <https://wammu.eu/docs/manual/c/api.html>`_
- `dummy driver <https://wammu.eu/docs/manual/testing/dummy-driver.html>`_


About SMS
---------

- `IMSI <https://en.wikipedia.org/wiki/International_mobile_subscriber_identity>`_: identifier of a SIM card, i.e. a 64-bit field designating a user (*International Mobile Subscriber Identity*)
- `SMSC <https://en.wikipedia.org/wiki/Short_Message_service_center>`_: SMS operator gateway (*Short Message Service Center*)
- `UDH <https://en.wikipedia.org/wiki/User_Data_Header>`_: optional binary SMS header (*User Data Header*)
- `SMS class <https://en.wikipedia.org/wiki/Data_Coding_Scheme#Message_Classes>`_



Troubleshooting Telecom-level Issues
====================================

Best is to test various keys on various USB ports of various computers running various operating systems, possibly with various SIM cards.

On GNU/Linux, being root and monitoring the system and Gammu logs (and/or using the ``--debug-file`` Gammu command-line option) should certainly help.



Please React!
=============

If you have information more detailed or more recent than those presented in this document, if you noticed errors, neglects or points insufficiently discussed, drop us a line! (for that, follow the Support_ guidelines).


Ending Word
===========

Have fun with Mobile - but do not spam people!

.. comment Mostly added to ensure there is at least one figure directive,
.. otherwise the LateX graphic support will not be included:

.. figure:: mobile-title.png
   :alt: Mobile logo
   :width: 50 %
   :align: center

:raw-html:`<a name="mobile_bottom"></a>`
