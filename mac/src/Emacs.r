/* Resource definitions for GNU Emacs on the Macintosh.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Contributed by Andrew Choi (akochoi@users.sourceforge.net).  */

#include "Types.r"
/* added for finder icon balloon help --ben */
#include "Balloons.r"

/* Define to use gnu icon */
/* #define GNU_ICON 1 */

resource 'STR#' (128) {
  {
    "TERM=macterm",
    "TERMCAP=macterm:co#80:li#40:up=up:do=do:le=le:nd=nd:cm=cm:cs=cs:ce=ce:cd=cd:cl=cl:al=al:dl=dl:",
    /* "HOME=/Ix/Data Files/Emacs Mac Port/emacs-20.4/mac/", */
    /* "MAIL=/Ix/System Folder/Eudora Folder/In" */
  }
};

resource 'STR#' (129) {
	{
		"emacs",
		"-l",
		"loadup"
	}
};

/* added for finder icon balloon help --ben */
resource 'hfdr' (-5696) { /*help for emacs icon*/
	/*header component*/
	HelpMgrVersion, hmDefaultOptions, 0, 0,
	{ /*icon component*/
		HMSTRResItem { /*use 'STR ' resource 128*/
			128
		}
   	}
};

/* added for finder icon balloon help --ben */
resource 'STR ' (128) { /*help message for emacs icon*/
	"GNU Emacs\0xd1the extensible, customizable, self-documenting real-time display editor."
};

resource 'MENU' (128, preload) {
	128,
	textMenuProc,
	0x7FFFFFFD,
	enabled,
	apple,
	{	/* array: 2 elements */
		/* [1] */
		"About Emacs\0xc9", noIcon, noKey, noMark, plain,
		/* [2] */
		"-", noIcon, noKey, noMark, plain
	}
};

resource 'MBAR' (128, "MBAR for Menus1", preload) {
	{	/* array MenuArray: 1 element */
		/* [1] */
		128
	}
};

resource 'WIND' (128, "Window", purgeable) {
	{68, 33, 554, 754},
	kWindowFullZoomGrowDocumentProc,
	invisible,
	goAway,
	0x0,
	"Terminal",
	kWindowStaggerMainScreen
};

resource 'WIND' (129, "Terminal window", purgeable) {
	{32, 8, 76, 620},
	kWindowModalDialogProc,
	invisible,
	goAway,
	0x0,
	"Terminal",
	kWindowDefaultPosition
};

resource 'WIND' (130, "Dialog window", purgeable) {
	{32, 8, 42, 18},
	kWindowModalDialogProc,
	invisible,
	goAway,
	0x0,
	"Terminal",
	kWindowDefaultPosition
};

resource 'ALRT' (128, "About Box", purgeable) {
	{40, 20, 160, 297},
	128,
	{	/* array: 4 elements */
		/* [1] */
		OK, visible, silent,
		/* [2] */
		OK, visible, silent,
		/* [3] */
		OK, visible, silent,
		/* [4] */
		OK, visible, silent
	},
	centerMainScreen
};

resource 'DITL' (128, purgeable) {
	{	/* array DITLarray: 2 elements */
		/* [1] */
		{88, 185, 108, 265},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{10, 60, 72, 278},
		StaticText {
			disabled,
			"GNU Emacs 21.0.99 for Mac OS\n"
                        "(27 February 2001 release)\n"
                        "Report bugs to emacs-pretest-bug@gnu.org"
		}
	}
};

resource 'ALRT' (129, "Ram Too Large", purgeable) {
	{40, 20, 160, 297},
	129,
	{	/* array: 4 elements */
		/* [1] */
		OK, visible, silent,
		/* [2] */
		OK, visible, silent,
		/* [3] */
		OK, visible, silent,
		/* [4] */
		OK, visible, silent
	},
	centerMainScreen
};

resource 'DITL' (129, purgeable) {
	{	/* array DITLarray: 2 elements */
		/* [1] */
		{88, 185, 108, 265},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{10, 60, 72, 278},
		StaticText {
			disabled,
			"Emacs does not run on a Macintosh with more than 256 MB of physical or virtual memory"
		}
	}
};

resource 'BNDL' (128) {
	'EMAx',
	0,
	{	/* array TypeArray: 2 elements */
		/* [1] */
		'FREF',
 		{	/* array IDArray: 2 elements */
  			/* [1] */
 			0, 128,
 			/* [2] */
 			1, 129
		},
		/* [2] */
		'ICN#',
 		{	/* array IDArray: 2 elements */
  			/* [1] */
 			0, 128,
 			/* [2] */
 			1, 129
		}
	}
};

resource 'FREF' (128) {
	'APPL',
	0,
	""
};

resource 'FREF' (129) {
	'TEXT',
	1,
	""
};

resource 'vers' (1) {
	0x1,
	0x0,
	development,
	0x0,
	0,
	"d6",
	"GNU Emacs 21.1 for Mac OS\n\0xa9 2"
	"000 Free Software Foundation"
};

data 'EMAx' (0, "Owner resource") {
	$"00"                                                 /* . */
};

#ifdef GNU_ICON
resource 'ICN#' (128) {
	{	/* array: 2 elements */
		/* [1] */
		$"0000 0000 0000 0000 0000 0080 0000 0040"
		$"0100 0030 0203 5808 0408 8400 0418 0220"
		$"0420 0008 0464 9C80 04C1 0018 0000 0004"
		$"118B 0120 000A 0000 0614 8180 0204 2080"
		$"0468 0080 0480 4480 0000 0080 0148 08C0"
		$"0120 00C0 0182 4020 0388 0120 0181 2040"
		$"01E8 B040 00E0 70C0 0078 9880 003A 9880"
		$"001E 4000 000F C0",
		/* [2] */
		$"0000 0000 0000 0100 0000 0180 0000 00E0"
		$"0100 3030 0203 F818 063F FC18 0E3F FE38"
		$"1C7F FF78 1CFF FFF8 1CFF FFF8 1E7F FFFC"
		$"1FFF FF38 1FFF FF80 0FFF FF80 07FF FF80"
		$"0FFF FF80 0FFF FFC0 01FF FFC0 03FF FFC0"
		$"03FF FFC0 03FF FFE0 03FF FFE0 03FF FFE0"
		$"01FF FFC0 01FF FFC0 00FF FFC0 007F FF80"
		$"001F E700 000F C000 0004 80"
	}
};
#else
resource 'ICN#' (128) {
	{	/* array: 2 elements */
		/* [1] */
		$"0000 0000 0000 0000 0001 F000 07DE 0F60"
		$"0860 0090 1200 0028 1200 0008 0800 0008"
		$"0800 0008 1000 0004 1000 0004 2000 0004"
		$"2000 0044 4018 12C2 4018 0002 4018 0082"
		$"4002 0C42 2000 1E02 2004 1E42 2004 0C02"
		$"2004 0042 2002 0082 5001 8F05 8800 7004"
		$"0800 0008 0400 0010 0200 0060 01C0 0380"
		$"003F FC00 2000 0000 4000 0000 80",
		/* [2] */
		$"0000 0000 0000 0000 0001 F000 07DF FF60"
		$"0FFF FFF0 1FFF FFF8 1FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 1FFF FFFC 1FFF FFFC 3FFF FFFC"
		$"3FFF FFFC 7FFF FFFE 7FFF FFFE 7FFF FFFE"
		$"7FFF FFFE 3FFF FFFE 3FFF FFFE 3FFF FFFE"
		$"3FFF FFFE 3FFF FFFE 7FFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
	}
};
#endif

#ifdef GNU_ICON
resource 'icl4' (128) {
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 000C 0000 0000"
	$"0000 0000 0000 0000 0000 000C F000 0000"
	$"0000 0000 0000 0000 0000 0000 CFD0 0000"
	$"0000 000F 0000 0000 00CC 0000 00FF 0000"
	$"0000 00F0 0000 00FF CFCF F000 000C F000"
	$"0000 0FD0 00CD FD0C F000 CF00 000C D000"
	$"0000 CFC0 00DF F000 C000 00F0 00FC C000"
	$"000C DF00 0DFD 0CD0 0000 DCDD 0DC0 F000"
	$"000D 0F00 DFFC 0F00 FDDF FFDD FC0D D000"
	$"000C 0F00 FFC0 DCDF 0000 DC00 00CF F000"
	$"000C 0CD0 0C0C CDC0 000C 0CCD CCCC CF00"
	$"000F 00CF F0DC FCFF DDC0 CDDF 00FD C000"
	$"000D D000 CDC0 FCFC 0CDC CCCD C000 0000"
	$"0000 CFF0 D00F 0F0C F0CD CCCF F000 0000"
	$"0000 0CFC 00CD 0F0D D0FD CCCD F000 0000"
	$"0000 CFCC DFF0 FC0C DCCC CCCC F000 0000"
	$"0000 DFC0 FDDC 0C0C DFC0 CEDC FC00 0000"
	$"0000 000C DDCC CDC0 C00C CCCC FC00 0000"
	$"0000 00CF CF0C FC0C 0000 F00D FF00 0000"
	$"0000 00CF D0FC 0CCC C000 0CCC FF00 0000"
	$"0000 00CF F0DC 0CFC CFCD DDDC DCF0 0000"
	$"0000 00FF FCD0 FDCC DDC0 00DF 0DF0 0000"
	$"0000 00CF FDCD 0DCF CDFC CC0D 0FC0 0000"
	$"0000 000F FFFC FCD0 F0FF C000 DF00 0000"
	$"0000 000C FFF0 CDCD DFFF DCCC FF00 0000"
	$"0000 0000 CFFF FC0D F0CF F000 FC00 0000"
	$"0000 0000 0CFF FDF0 F0DF FDDD F000 0000"
	$"0000 0000 000F FFFC CFC0 0DDD 0000 0000"
	$"0000 0000 0000 FFFF FF00 0000 0000 0000"
	$"0000 0000 0000 0C00 C0"
};
#else
resource 'icl4' (128) {
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 000F FFFF 0000 0000 0000"
	$"0000 0FFF FF0F FFF0 0000 FFFF 0FF0 0000"
	$"0000 FCCC CFF0 0000 0000 0000 FCCF 0000"
	$"000F 0DED CC00 0000 0000 0000 0DEC F000"
	$"000F 0DED C000 0000 0000 0000 00DD F000"
	$"0000 F0D0 0000 0000 0000 0000 0000 F000"
	$"0000 FC00 0000 0000 0000 0000 0000 F000"
	$"000F 0000 0000 0000 0000 0000 0000 0F00"
	$"000F 0000 0000 0000 0000 0000 0000 0F00"
	$"00F0 0000 0000 0000 0000 0000 0000 0F00"
	$"00F0 0000 0000 0000 0000 0000 DEC0 0F00"
	$"0F00 0000 00CE EC00 0CCC CCCC FFC0 00F0"
	$"0F00 0000 0CDF FD0C C000 000D CDC0 00F0"
	$"0F00 0000 00CE ECD0 0000 CC00 D000 00F0"
	$"0F00 0000 0000 00D0 000C EEC0 0D00 00F0"
	$"00F0 0000 0000 0D00 00CE AAEC 0D00 00F0"
	$"00F0 0000 0000 0D00 00CE AAEC 0D00 00F0"
	$"00F0 0000 0000 0D00 000C EEC0 0D00 00F0"
	$"00F0 0000 0000 0D00 0000 CC00 0D00 00F0"
	$"00F0 0000 0000 0CD0 0000 0000 E000 00F0"
	$"0F0F 0000 0000 00CD D000 EEEE 0000 0F0F"
	$"F000 F000 0000 0000 0EEE 0000 0000 0F00"
	$"0000 F000 0000 0000 0000 0D00 0000 FC00"
	$"0000 0F00 0000 0000 0000 0D00 000F C000"
	$"0000 00F0 0000 0000 0000 0C00 0FFC C000"
	$"0000 C00F FF00 0000 0000 00EE ECCC 0000"
	$"000D 0000 0CFF FFFF FEEE EECC CCC0 0000"
	$"0CF0 0000 0000 CCCC CCCC CC0C 0C00 0C00"
	$"CEC0 0000 0000 0000 0000 0000 0000 0DC0"
	$"FD00 0000 0000 0000 0000 0000 0000 00D0"
};
#endif

#ifdef GNU_ICON
resource 'icl8' (128) {
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 002B 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 002B FF00 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 2BFF F900 0000 0000"
	$"0000 0000 0000 00FF 0000 0000 0000 0000"
	$"0000 2B2B 0000 0000 0000 FFFF 0000 0000"
	$"0000 0000 0000 FF00 0000 0000 0000 FFFF"
	$"2BFF 2BFF FF00 0000 0000 002B FF00 0000"
	$"0000 0000 00FF F900 0000 2BF9 FFF9 002B"
	$"FF00 0000 2BFF 0000 0000 002B F900 0000"
	$"0000 0000 2BFF 2B00 0000 F9FF FF00 0000"
	$"2B00 0000 0000 FF00 0000 FF2B 2B00 0000"
	$"0000 002B F9FF 0000 00F9 FFF9 002B F900"
	$"0000 0000 F92B F9F9 00F9 2B00 FF00 0000"
	$"0000 00F9 00FF 0000 F9FF FF2B 00FF 0000"
	$"FFF9 F9FF FFFF F9F9 FF2B 00F9 F900 0000"
	$"0000 002B 00FF 0000 FFFF 2B00 F92B F9FF"
	$"0000 0000 F92B 0000 0000 2BFF FF00 0000"
	$"0000 002B 002B F900 002B 002B 2BF9 2B00"
	$"0000 002B 002B 2BF9 2B2B 2B2B 2BFF 0000"
	$"0000 00FF 0000 2BFF FF00 F92B FF2B FFFF"
	$"F9F9 2B00 2BF9 F9FF 0000 FFF9 2B00 0000"
	$"0000 00F9 F900 0000 2BF9 2B00 FF2B FF2B"
	$"002B F92B 2B2B 2BF9 2B00 0000 0000 0000"
	$"0000 0000 2BFF FF00 F900 00FF 00FF 002B"
	$"FF00 2BF9 2B2B 2BFF FF00 0000 0000 0000"
	$"0000 0000 002B FF2B 0000 2BF9 00FF 00F9"
	$"F900 FFF9 2B2B 2BF9 FF00 0000 0000 0000"
	$"0000 0000 2BFF 2B2B F9FF FF00 FF2B 002B"
	$"F92B 2B2B 2B2B 2B2B FF00 0000 0000 0000"
	$"0000 0000 F9FF 2B00 FFF9 F92B 002B 002B"
	$"F9FF 2B00 2BFC F92B FF2B 0000 0000 0000"
	$"0000 0000 0000 002B F9F9 2B2B 2BF9 2B00"
	$"2B00 002B 2B2B 2B2B FF2B 0000 0000 0000"
	$"0000 0000 0000 2BFF 2BFF 002B FF2B 002B"
	$"0000 0000 FF00 00F9 FFFF 0000 0000 0000"
	$"0000 0000 0000 2BFF F900 FF2B 002B 2B2B"
	$"2B00 0000 002B 2B2B FFFF 0000 0000 0000"
	$"0000 0000 0000 2BFF FF00 F92B 002B FF2B"
	$"2BFF 2BF9 F9F9 F92B F92B FF00 0000 0000"
	$"0000 0000 0000 FFFF FF2B F900 FFF9 2B2B"
	$"F9F9 2B00 0000 F9FF 00F9 FF00 0000 0000"
	$"0000 0000 0000 2BFF FFF9 2BF9 00F9 2BFF"
	$"2BF9 FF2B 2B2B 00F9 00FF 2B00 0000 0000"
	$"0000 0000 0000 00FF FFFF FF2B FF2B F900"
	$"FF00 FFFF 2B00 0000 F9FF 0000 0000 0000"
	$"0000 0000 0000 002B FFFF FF00 2BF9 2BF9"
	$"F9FF FFFF F92B 2B2B FFFF 0000 0000 0000"
	$"0000 0000 0000 0000 2BFF FFFF FF2B 00F9"
	$"FF00 2BFF FF00 0000 FF2B 0000 0000 0000"
	$"0000 0000 0000 0000 002B FFFF FFF9 FF00"
	$"FF00 F9FF FFF9 F9F9 FF00 0000 0000 0000"
	$"0000 0000 0000 0000 0000 00FF FFFF FF2B"
	$"2BFF 2B00 00F9 F9F9 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 FFFF FFFF"
	$"FFFF 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 002B 0000"
	$"2B"
};
#else
resource 'icl8' (128) {
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 00FF"
	$"FFFF FFFF 0000 0000 0000 0000 0000 0000"
	$"0000 0000 00FF FFFF FFFF F5FF FFFF FFF5"
	$"F5F5 F5F5 FFFF FFFF 00FF FF00 0000 0000"
	$"0000 0000 FFF6 F6F6 F6FF FFF5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 FFF7 F6FF 0000 0000"
	$"0000 00FF F5F9 FBF9 F7F7 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F9 FCF6 FF00 0000"
	$"0000 00FF F5F9 FBF9 F7F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F9F9 FF00 0000"
	$"0000 0000 FFF5 F9F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 FF00 0000"
	$"0000 0000 FFF7 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 FF00 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"0000 00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5FF 0000"
	$"0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F9FC F6F5 F5FF 0000"
	$"00FF F5F5 F5F5 F5F5 F5F5 F7FB FBF7 F5F5"
	$"F5F6 F6F7 F7F7 F7F8 FFFF F7F5 F5F5 FF00"
	$"00FF F5F5 F5F5 F5F5 F5F6 F9FF FFF9 F5F7"
	$"F7F5 F5F5 F5F5 F5F9 F7F9 F6F5 F5F5 FF00"
	$"00FF F5F5 F5F5 F5F5 F5F5 F7FB FCF7 F9F5"
	$"F5F5 F5F5 F6F6 F5F5 F9F5 F5F5 F5F5 FF00"
	$"00FF F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F9F5"
	$"F5F5 F5F7 FBFB F7F5 F5F9 F5F5 F5F5 FF00"
	$"0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F9 F5F5"
	$"F5F5 F7FB FDFD ACF7 F5F9 F5F5 F5F5 FF00"
	$"0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F9 F5F5"
	$"F5F5 F7FB FDFD ACF7 F5F9 F5F5 F5F5 FE00"
	$"0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F9 F5F5"
	$"F5F5 F5F7 FBFB F7F5 F5F9 F5F5 F5F5 FE00"
	$"0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F9 F5F5"
	$"F5F5 F5F5 F7F7 F5F5 F5FA F5F5 F5F5 FE00"
	$"0000 FFF5 F5F5 F5F5 F5F5 F5F5 F5F7 F9F5"
	$"F5F5 F5F5 F5F5 F5F5 FBF5 F5F5 F5F5 FF00"
	$"00FF F5FF F5F5 F5F5 F5F5 F5F5 F5F5 F7F9"
	$"F9F5 F5F5 FBFB FBFB F5F5 F5F5 F5FE F5FF"
	$"FFF5 F5F5 FFF5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5FB FBFB F5F5 F5F5 F5F5 F5F5 F5FF F5F5"
	$"F5F5 F5F5 FFF5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F9 F5F5 F5F5 F5F5 FFF6 F5F5"
	$"F5F5 F5F5 F5FF F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F9 F5F5 F5F5 F5FF F6F5 F5F5"
	$"F5F5 F5F5 F5F5 FFF5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F6 F5F5 F5FF FFF7 F6F5 F5F5"
	$"F5F5 F5F5 F7F5 F5FF FFFF F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 ACAC ACF7 F6F6 F5F5 F5F5"
	$"F5F5 F5F9 F5F5 F5F5 F5F7 FFFF FFFF FFFF"
	$"FFFC FCAC ACAC F6F6 F7F6 F6F5 F5F5 F5F5"
	$"F5F6 FFF5 F5F5 F5F5 F5F5 F5F5 F6F7 F6F7"
	$"F6F7 F6F6 F6F6 F5F6 F5F6 F5F5 F5F7 F5F5"
	$"F6FC F7F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F9 F7F5"
	$"FFF9 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F9F5"
};
#endif

#ifdef GNU_ICON
resource 'ics#' (128) {
	{	/* array: 2 elements */
		/* [1] */
		$"0000 0008 11E6 26B4 2EEA 2906 5B14 36D8"
		$"2EA8 1A28 1D8C 1B5C 1EC8 0FE8 0780",
		/* [2] */
		$"0010 001C 11FE 37FE 7FFE 7FFE 7FFE 3FF8"
		$"3FF8 1FF8 1FFC 1FFC 1FF8 0FF8 07F0 0280"
	}
};
#else
resource 'ics#' (128) {
	{	/* array: 2 elements */
		/* [1] */
		$"0000 0000 0001 604B 6000 6002 0831 0078"
		$"1079 1030 1001 0802 063C 01C0",
		/* [2] */
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
	}
};
#endif

#ifdef GNU_ICON
resource 'ics4' (128) {
	$"0000 0000 000C 0000 0000 0000 000C FD00"
	$"000F 000F FFF0 0FF0 00FD 0FFC F0FF 0FD0"
	$"0DF0 FFFD FFFD FDF0 0CFD FCDF 0CDD CFF0"
	$"0FDF FDFF DDDF CFC0 00FF DFFD FFCF F000"
	$"00FC FFFC FCED F000 000F FCFC CCFD F000"
	$"000F FFCF FDDD FF00 000F FDFF DFCF FF00"
	$"000F FFFD FFDC F000 0000 FFFF FFFD F000"
	$"0000 0FFF FCDD 0000 0000 00C0 C0"
};
#else
resource 'ics4' (128) {
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 00DE CEEC 000C CCCC CCFF"
	$"DFFD 0CC0 0000 0DCD CEEC D000 00CC 00D0"
	$"0000 D000 0CEE C00D 000D 0000 CEAA EC0D"
	$"000D 0000 CEAA EC0D 000D 0000 0CEE C00D"
	$"000D 0000 00CC 000D 000C D000 0000 00E0"
	$"0000 CDD0 00EE EE00 0000 000E EE00 0000"
	$"0000 0000 000D 0000 0000 0000 000D"
};
#endif

#ifdef GNU_ICON
resource 'ics8' (128) {
	$"0000 0000 0000 0000 0000 002B 0000 0000"
	$"0000 0000 0000 0000 0000 002B FFF9 0000"
	$"0000 00FF 0000 00FF FFFF FF00 00FF FF00"
	$"0000 FFF9 00FF FF2B FF00 FFFF 00FF F900"
	$"00F9 FF00 FFFF FFF9 FFFF FFF9 FFF9 FF00"
	$"002B FFF9 FF2B F9FF 002B F9F9 2BFF FF00"
	$"00FF F9FF FFF9 FFFF F9F9 F9FF 2BFF 2B00"
	$"0000 FFFF F9FF FFF9 FFFF 2BFF FF00 0000"
	$"0000 FF2B FFFF FF2B FF2B FCF9 FF00 0000"
	$"0000 00FF FF2B FF2B 2B2B FFF9 FF00 0000"
	$"0000 00FF FFFF 2BFF FFF9 F9F9 FFFF 0000"
	$"0000 00FF FFF9 FFFF F9FF 2BFF FFFF 0000"
	$"0000 00FF FFFF FFF9 FFFF F92B FF00 0000"
	$"0000 0000 FFFF FFFF FFFF FFF9 FF00 0000"
	$"0000 0000 00FF FFFF FF2B F9F9 0000 0000"
	$"0000 0000 0000 2B00 2B"
};
#else
resource 'ics8' (128) {
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F5F5 F9FC"
	$"F7FB FBF7 F5F5 F5F6 F6F7 F7F7 F7F8 FFFF"
	$"F9FF FFF9 F5F7 F7F5 F5F5 F5F5 F5F9 F7F9"
	$"F7FB FCF7 F9F5 F5F5 F5F5 F6F6 F5F5 F9F5"
	$"F5F5 F5F5 F9F5 F5F5 F5F7 FBFB F7F5 F5F9"
	$"F5F5 F5F9 F5F5 F5F5 F7FB FDFD ACF7 F5F9"
	$"F5F5 F5F9 F5F5 F5F5 F7FB FDFD ACF7 F5F9"
	$"F5F5 F5F9 F5F5 F5F5 F5F7 FBFB F7F5 F5F9"
	$"F5F5 F5F9 F5F5 F5F5 F5F5 F7F7 F5F5 F5FA"
	$"F5F5 F5F7 F9F5 F5F5 F5F5 F5F5 F5F5 FBF5"
	$"F5F5 F5F5 F7F9 F9F5 F5F5 FBFB FBFB F5F5"
	$"F5F5 F5F5 F5F5 F5FB FBFB F5F5 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F9 F5F5 F5F5"
	$"F5F5 F5F5 F5F5 F5F5 F5F5 F5F9 F5F5 F5F5"
};
#endif

resource 'ICN#' (129) {
	{	/* array: 2 elements */
		/* [1] */
		$"0000 0300 0000 1C80 0000 E0B0 0007 00F0"
		$"0038 0070 01C0 0038 0E00 8038 7008 4038"
		$"8008 2038 8004 0038 8080 041C C086 301C"
		$"C087 311C 4007 BB8C 6167 378C 6170 070E"
		$"2130 6006 3031 FC06 3047 FE06 10E7 FF02"
		$"1867 FF82 1847 FF03 080F FE07 0C1F E03C"
		$"0C0F C1F0 0406 0F80 0400 7C00 0203 E000"
		$"031F 0000 01F8 0000 00C0",
		/* [2] */
		$"0000 0300 0000 1F80 0000 FFB0 0007 FFF0"
		$"003F FFF0 01FF FFF8 0FFF FFF8 7FFF FFF8"
		$"FFFF FFF8 FFFF FFF8 FFFF FFFC FFFF FFFC"
		$"FFFF FFFC 7FFF FFFC 7FFF FFFC 7FFF FFFE"
		$"3FFF FFFE 3FFF FFFE 3FFF FFFE 1FFF FFFE"
		$"1FFF FFFE 1FFF FFFF 0FFF FFFF 0FFF FFFC"
		$"0FFF FFF0 07FF FF80 07FF FC00 03FF E000"
		$"03FF 0000 01F8 0000 00C0"
	}
};

resource 'icl4' (129) {
	$"0000 0000 0000 0000 0000 00FF 0000 0000"
	$"0000 0000 0000 0000 000F FF0C F000 0000"
	$"0000 0000 0000 0000 FFF0 0000 F0FF 0000"
	$"0000 0000 0000 0FFF 0000 0000 FFDF 0000"
	$"0000 0000 00FF F000 0000 0000 CFEF 0000"
	$"0000 000F FF00 0000 0000 0000 00ED F000"
	$"0000 FFF0 0000 0000 DC00 0000 00AD F000"
	$"0FFF 0000 0000 E000 0DC0 0000 00FE F000"
	$"F000 0000 0000 DC00 0CD0 0000 00FE F000"
	$"F000 0000 CC00 CC00 00C0 0C00 00FA F000"
	$"F000 0000 D000 00C0 00C0 0DCC 00CF EF00"
	$"EF00 0000 E000 0EED C0ED C0DD 000F EF00"
	$"EFC0 0000 DC00 CEEF CCEF D0DD 000F AF00"
	$"DFC0 000C 00C0 CEFE D0EF D0ED E00C FF00"
	$"0EFC 00CD 0EDC 0DDE 00DF CDEE EC00 FF00"
	$"0EFC 000E 0DEE 00CC 00CC CDFE D000 FFF0"
	$"0DFC 000D CCFE C00C DEDD 0CDC 0000 CFF0"
	$"00EF C00C 0CDE 00CE FFFF AE00 0000 0FF0"
	$"00EF C000 DD0C 0DEE FFFF FEED C000 0FF0"
	$"00DF C000 DFE0 CDFF FFFF FFAF C000 0CF0"
	$"000E FC00 DEE0 CAFF FFFF FFFE D000 0CFD"
	$"000E FC00 CED0 DEFF FFFF FFFE C000 0CFF"
	$"000D FC00 0C0C EFFF FFFF AEDD 00CC CFFF"
	$"0000 EF00 000D EFFF FADD C00C CCFF FED0"
	$"0000 EF00 000C FAFF AED0 CCCF FFEE D000"
	$"0000 DF00 000C DEED CCCC FFFE ED00 0000"
	$"0000 0E00 0000 0CCC CFFF EED0 0000 0000"
	$"0000 0DFD 0000 CCFF FEED 0000 0000 0000"
	$"0000 00EF DDDF FFEE D000 0000 0000 0000"
	$"0000 000E FFFE ED00 0000 0000 0000 0000"
	$"0000 0000 DDD0"
};

resource 'icl8' (129) {
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 FFFF 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 00FF FFFF 00F7 FF00 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"FFFF FF00 0000 0000 FF00 FFFF 0000 0000"
	$"0000 0000 0000 0000 0000 0000 00FF FFFF"
	$"0000 0000 0000 0000 FFFF FAFF 0000 0000"
	$"0000 0000 0000 0000 0000 FFFF FF00 0000"
	$"0000 0000 0000 0000 F7FF FCFF 0000 0000"
	$"0000 0000 0000 00FF FFFF 0000 0000 0000"
	$"F500 0000 0000 0000 0000 FCFA FF00 0000"
	$"0000 0000 FFFF FF00 0000 0000 0000 0000"
	$"812B 0000 0000 0000 0000 FDFA FF00 0000"
	$"00FF FFFF 0000 0000 0000 0000 FBF5 0000"
	$"0081 2B00 0000 0000 0000 FFFC FF00 0000"
	$"FF00 0000 0000 0000 00F5 0000 F9F8 0000"
	$"00F8 FA00 0000 0000 0000 FFFC FF00 0000"
	$"FF00 0000 0000 0000 F8F8 0000 F7F8 0000"
	$"00F5 2B00 002B 0000 0000 FFFD FF00 0000"
	$"FF00 0000 0000 00F5 FA00 0000 00F5 F800"
	$"0000 2B00 00FA F8F6 0000 F7FF FCFF 0000"
	$"FCFF 0000 0000 0000 FB00 0000 00FB FCF9"
	$"F600 FBFA F700 5656 0000 00FF FCFF 0000"
	$"FCFF F700 0000 0000 81F6 0000 F8FC ACFE"
	$"F72B ACFE 5600 F9FA 0000 00FF FDFF 0000"
	$"F9FF F700 0000 F5F7 F500 F600 2BAC FEFC"
	$"FA00 FCFF 81F5 FB81 FBF5 00F7 FFFF 0000"
	$"00FC FFF6 0000 2B81 00FC 81F8 0081 81FC"
	$"F500 FAFF 2B81 ACFC FCF6 0000 FFFF 0000"
	$"00FC FFF6 0000 00FB 0081 FCAC F500 F82B"
	$"0000 F8F8 2B81 FFAC F900 0000 FFFF FF00"
	$"00F9 FFF6 0000 0081 F6F8 FFAC F700 00F8"
	$"F9FC FA56 00F6 562B 0000 0000 F7FF FF00"
	$"0000 FCFF F700 00F7 002B FAFB F500 F7FC"
	$"FEFF FFFF FDFB F500 0000 0000 00FF FF00"
	$"0000 FCFF F700 0000 F9FA F5F8 0081 FCAC"
	$"FFFF FFFF FFFC AC56 2B00 0000 00FF FF00"
	$"0000 F9FF F700 0000 81FF AC00 F781 FFFF"
	$"FFFF FFFF FFFF FDFE F800 0000 00F7 FF00"
	$"0000 00FC FFF7 0000 56FC FBF5 F7FD FFFF"
	$"FFFF FFFF FFFF FFAC 8100 0000 00F6 FFF9"
	$"0000 00FC FFF7 0000 2BAC 5600 F9FC FFFF"
	$"FFFF FFFF FFFF FFFB F700 0000 00F7 FFFF"
	$"0000 00F9 FFF7 0000 F5F6 002B FBFE FFFF"
	$"FFFF FFFE FDFC FA56 0000 F7F7 F7FF FFFF"
	$"0000 0000 FCFF 0000 0000 00FA ACFF FFFF"
	$"FFFD 8156 2BF5 00F7 F7F7 FFFF FFFC F900"
	$"0000 0000 FCFF 0000 0000 00F7 FEFD FFFE"
	$"FDFC F900 F7F7 F7FF FFFF FCFC F900 0000"
	$"0000 0000 F9FF 0000 0000 00F6 56AC FBF9"
	$"F7F6 F7F7 FFFF FFFC FCF9 0000 0000 0000"
	$"0000 0000 00FC 0000 0000 0000 002B F7F7"
	$"F7FF FFFF FCFC F900 0000 0000 0000 0000"
	$"0000 0000 00F9 FFF9 0000 0000 F7F7 FFFF"
	$"FFFC FCF9 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 FCFF F9F9 F9FF FFFF FCFC"
	$"F900 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 00FC FFFF FFFC FCF9 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 FAFA F9"
};

resource 'ics#' (129) {
	{	/* array: 2 elements */
		/* [1] */
		$"0030 01CC 0E04 7006 8006 8006 8352 4412"
		$"44C3 49F1 2BF1 23C7 23B8 01C0 1E",
		/* [2] */
		$"0030 01FC 0FFC 7FFE FFFE FFFE FFFE 7FFE"
		$"7FFF 7FFF 3FFF 3FFF 3FF8 3FC0 1E"
	}
};

resource 'ics4' (129) {
	$"0000 0000 0CFF C000 0000 0CCF FFC0 EE00"
	$"00CC FFFC 0000 CEC0 CFFF C0C0 C000 0ED0"
	$"F000 C0C0 CC00 0FD0 F000 C0CD 0CCC 0DA0"
	$"FC0C C0DF CACD CCF0 DF0C DFCD 0DDA C0FC"
	$"CF0C CD0C EEDC 00EF 0FC0 FCCF FFFE C0DF"
	$"0DF0 DCDF FFFA C0CF 0CF0 0CAF FEDC CFFD"
	$"00F0 0CEE DCFF FDC0 00CD 00CF FFDC 0000"
	$"000D FFFD C000 0000 0000 CC"
};

resource 'ics8' (129) {
	$"0000 0000 0000 0000 F5F7 FFFF 2BF5 0000"
	$"0000 0000 00F6 F7FF FFFF F600 ACAC 0000"
	$"0000 F6F7 FFFF FFF6 0000 0000 F8AC F700"
	$"2BFF FFFF F600 F600 F8F5 0000 00AC FA00"
	$"FFF5 0000 F6F5 F700 F6F6 F500 F5FE FA00"
	$"FFF5 00F5 F700 F756 F5F8 2BF7 0081 FDF5"
	$"FF2B 00F6 F700 FAFE F7FD F8FA 2BF7 FF00"
	$"56FF 00F7 56FF 2B56 F556 56FD F800 FEF7"
	$"2BFF F5F6 F7FA F5F8 ACFC F92B 0000 FBFF"
	$"00FF F700 FFF8 F8FE FFFF FFFC F600 56FF"
	$"0056 FF00 FAF7 81FF FFFF FFFD 2B00 F6FF"
	$"002B FFF5 002B FDFF FFFC 56F7 F7FF FF81"
	$"0000 FFF5 00F6 FCFB FAF7 FFFF FFF9 F600"
	$"0000 F8F9 0000 F7FF FFFF F9F6 0000 0000"
	$"0000 0081 FFFF FFF9 F600 0000 0000 0000"
	$"0000 0000 F8F6"
};
