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
  SVersion = 'GSXSteg Version %s';
  SCopy = 'Copyright (C) 2007-2008, Stefan Ascher';
  SInfo = 'Teil-Programm von GPGSX'#13#10 +
    'http://stievie.bplaced.net/gpgsx/'#13#10 +
    #13#10 +
    'Lizensiert unter der GPL.'#13#10 +
    'BENUTZUNG AUF EIGENE GEFAHR !';
  SHelp = 'Aufruf:'#13#10 +
    '  gsxsteg [-batch] -<aktion> [-<argument> <wert>]'#13#10 +
    #13#10 +
    '  batch:   Batch-Modus, alles muss in Kommandozeilenargumenten angegeben werden'#13#10 +
    'aktion kann sein:'#13#10 +
    '  hide:    Daten in einem Medium verbergen'#13#10 +
    '  extract: Daten aus einem Medium extrahieren'#13#10 +
    'argument kann sein:'#13#10 +
    '  medium:  Die Medium-Datei'#13#10 +
    '  dest:    Die Ziel-Datei (optional)'#13#10 +
    '  msg:     Dateiname mit zu verbergender Mitteilung'#13#10 +
    '  pass:    Passwort'#13#10 +
    'Beispiel:'#13#10 +
    '  gsxsteg -batch -extract -medium meinmedium.png -dest diedaten.txt -pass "mein geheimes passwort"';

  SCapacity = 'Kapazität: %d Byte';
  SMediumDoesNotExist = 'Medium existiert nicht';
  SFileSize = 'Dateigrösse: %d Byte';
  SHidefileDosNotExist = 'Zu verbergende Datei existiert nicht';
  SWrongArgs = 'Falsche oder fehlende Argumente';

  // Values
  StrMedium = 'Medium';
  StrDestination = 'Ziel';
  StrHide = 'Verbergen';
  StrPassword = 'Passwort';

  SPasswordEmpty = 'Passwort darf nicht leer sein.';
  SPicTooSmall = 'Bild ist zu klein.';
  SBitmapEmpty = 'Bitmap darf nicht leer sein.';
  SNothingToHide = 'Es gibt nichts zu verbergen.';
  SNoData = 'Es existieren keine Daten zu diesem Passwort.';
  SUnsuppPictrue = 'Nicht bekanntes Bild-Datei Format.';
  SUnsuppPixelFormat = 'Nicht bekanntes Pixel Format, 24 Bit notwendig.';
  SSuppFilter = 'Bekannte Formate';
