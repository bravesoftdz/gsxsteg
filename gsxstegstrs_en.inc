{
  GPGSX
  Copyright (C) 2008, Ascher Stefan. All rights reserved.
  stievie@inode.at, http://stievie.bplaced.net/gpgsx/

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
  
 $Id: $
}

resourcestring
  SVersion = 'gsxsteg Version %s';
  SCopy = 'Copyright (C) 2007-2008, Stefan Ascher';
  SInfo = 'License GPL.';
  SHelp = 'Usage:'#13#10 +
    '  gsxsteg [-batch] -<action> [-<argument> <value>]'#13#10 +
    #13#10 +
    '  batch:   Batch mode, everything must be given with commandline arguments'#13#10 +
    'action can be:'#13#10 +
    '  hide:    Hide data inside a medium'#13#10 +
    '  extract: Extract data from a medium'#13#10 +
    'argument can be:'#13#10 +
    '  medium:  The medium file'#13#10 +
    '  dest:    The destination file'#13#10 +
    '  msg:     Filename with message to hide or message'#13#10 +
    '  pass:    Password'#13#10 +
    'Example:'#13#10 +
    '  gsxsteg -batch -extract -medium mymedium.png -dest thedata.txt -pass "my secret pass"';

  SCapacity = 'Capacity: %d Byte';
  SMediumDoesNotExist = 'Medium does not exist';
  SFileSize = 'Filesize: %d Byte';
  SHidefileDosNotExist = 'File to hide does not exist';
  SWrongArgs = 'Wrong or missing arguments';

  // Values
  StrMedium = 'Medium';
  StrDestination = 'Destination';
  StrHide = 'Hide';
  StrPassword = 'Password';

