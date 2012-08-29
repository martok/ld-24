{
TextSuite (C) Steffen Xonna (aka Lossy eX)
http://www.opengl24.de/
-----------------------------------------------------------------------
For copyright informations see file copyright.txt.
}

{$I TextSuiteOptions.inc}

unit TextSuiteTTFUtils;

interface

uses Classes;

const
  NAME_ID_COPYRIGHT  = 0;
  NAME_ID_FACE_NAME  = 1;
  NAME_ID_STYLE_NAME = 2;
  NAME_ID_FULL_NAME  = 4;



  function MakeTTTableName(ch1, ch2, ch3, ch4: Char): Cardinal;
  function GetTTString(pBuffer: Pointer; BufferSize: Integer; NameID, LanguageID: Cardinal; var Text: AnsiString): Boolean;

  function GetTTFontFullNameFromStream(Stream: TStream; LanguageID: Cardinal): AnsiString;
  function GetTTFontFullNameFromFile(Filename: AnsiString; LanguageID: Cardinal): AnsiString;

(*
  function GetTTUnicodeGlyphIndex(DC: Cardinal; ch: Word): Word;
  function GetTTUnicodeCharCount(DC: Cardinal): Word;
*)

implementation


uses
  SysUtils,
  TextSuiteWideUtils,
  TextSuiteImports;


function SWAPWORD(x: Word): Word;
{$ifdef TS_PURE_PASCAL}
begin
  Result := x and $FF;
  Result := Result shl 8;
  Result := Result or (x shr 8);
{$else}
asm
  mov dl, al
  mov al, ah
  mov ah, dl
{$endif}
end;


function SWAPLONG(x: Cardinal): Cardinal;
{$ifdef TS_PURE_PASCAL}
begin
  Result := (x and $FF) shl 24;
  x := x shr 8;

  Result := Result or ((x and $FF) shl 16);
  x := x shr 8;
  
  Result := Result or ((x and $FF) shl 8);
  x := x shr 8;

  Result := Result or x;
{$else}
asm
  mov dx, ax
  shr eax, 16
  mov cx, ax
  mov al, dh
  mov ah, dl
  shl eax, 16
  mov al, ch
  mov ah, cl
{$endif}
end;


function MakeTTTableName(ch1, ch2, ch3, ch4: Char): Cardinal;
begin
  Result := ord(ch4) shl 24 or ord(ch3) shl 16 or ord(ch2) shl 8 or ord(ch1); 
end;


type
  TT_OFFSET_TABLE = packed record
  	uMajorVersion: Word;
  	uMinorVersion: Word;
	  uNumOfTables: Word;
  	uSearchRange: Word;
  	uEntrySelector: Word;
	  uRangeShift: Word;
  end;


  TT_TABLE_DIRECTORY = packed record
  	TableName: Cardinal;     // table name
  	uCheckSum: Cardinal;  // Check sum
	  uOffset: Cardinal;    // Offset from beginning of file
	  uLength: Cardinal;    // length of the table in bytes
  end;


  TT_NAME_TABLE_HEADER = packed record
  	uFSelector: Word;     //format selector. Always 0
	  uNRCount: Word;       //Name Records count
	  uStorageOffset: Word; //Offset for strings storage, from start of the table
  end;

  TT_NAME_RECORD = packed record
  	uPlatformID: Word;
	  uEncodingID: Word;
	  uLanguageID: Word;
	  uNameID: Word;
	  uStringLength: Word;
	  uStringOffset: Word;  //from start of storage area
  end;


const
  PLATFORM_ID_APPLE_UNICODE = 0;
  PLATFORM_ID_MACINTOSH     = 1;
  PLATFORM_ID_MICROSOFT     = 3;


function GetTTTableData(Stream: TStream; TableName: Cardinal; pBuff: Pointer; var Size: Integer): Boolean;
var
  Pos: Int64;
  OffsetTable: TT_OFFSET_TABLE;
  TableDir: TT_TABLE_DIRECTORY;
  Idx: Integer;
begin
  Result := False;

  Pos := Stream.Position;

  // Reading table header
  Stream.Read(OffsetTable, sizeof(TT_OFFSET_TABLE));
  OffsetTable.uNumOfTables := SWAPWORD(OffsetTable.uNumOfTables);
  OffsetTable.uMajorVersion := SWAPWORD(OffsetTable.uMajorVersion);
  OffsetTable.uMinorVersion := SWAPWORD(OffsetTable.uMinorVersion);

  //check is this is a true type font and the version is 1.0
  if (OffsetTable.uMajorVersion <> 1) or (OffsetTable.uMinorVersion <> 0) then
    Exit;

  // seaching table with name
  for Idx := 0 to OffsetTable.uNumOfTables -1 do begin
    Stream.Read(TableDir, sizeof(TT_TABLE_DIRECTORY));

    if (TableName = TableDir.TableName) then begin
      TableDir.uOffset := SWAPLONG(TableDir.uOffset);
      TableDir.uLength := SWAPLONG(TableDir.uLength);

      // copying tabledata
      if (pBuff <> nil) and (Size >= Integer(TableDir.uLength)) then begin
        Stream.Seek(TableDir.uOffset, soBeginning);
        Size := Stream.Read(pBuff^, TableDir.uLength);

        Result := Size = Integer(TableDir.uLength);
      end else

      begin
        // restoring streamposition
        Stream.Position := Pos;

        Size := TableDir.uLength;
        Result := True;
      end;

      break;
    end;
  end;
end;


function GetTTString(pBuffer: Pointer; BufferSize: Integer; NameID, LanguageID: Cardinal; var Text: AnsiString): Boolean;
var
  pActBuffer: pByte; 
  ttNTHeader: TT_NAME_TABLE_HEADER;
  ttRecord: TT_NAME_RECORD;
  Idx: Integer;
  Prio: Integer;

  procedure ExtractName;
  var
    pTempBuffer: pByte;
    pTemp: pWideChar;
    uStringLengthH2: Word;

    procedure SwapText(pText: pWideChar; Length: Word);
    begin
      while Length > 0 do begin
        pWord(pText)^ := SWAPWORD(pWord(pText)^);
        Inc(pText);
        Dec(Length);
      end;
    end;

  begin
    Result := True;

    ttRecord.uStringLength := SWAPWORD(ttRecord.uStringLength);
    ttRecord.uStringOffset := SWAPWORD(ttRecord.uStringOffset);

    uStringLengthH2 := ttRecord.uStringLength shr 1;

    pTempBuffer := pBuffer;
    Inc(pTempBuffer, ttNTHeader.uStorageOffset + ttRecord.uStringOffset);

    // Unicode
    if ((ttRecord.uPlatformID = PLATFORM_ID_MICROSOFT) and (ttRecord.uEncodingID in [0, 1])) or
       ((ttRecord.uPlatformID = PLATFORM_ID_APPLE_UNICODE) and (ttRecord.uEncodingID > 0)) then begin
      pTemp := tsStrAlloc(uStringLengthH2);
      try
        // uStringLengthH2 * 2 because possible buffer overrun
        Move(pTempBuffer^, pTemp^, uStringLengthH2 * 2);

        SwapText(pTemp, uStringLengthH2);

        WideCharLenToStrVar(pTemp, uStringLengthH2, Text);
      finally
        tsStrDispose(pTemp);
      end;
    end else

    // none unicode
    begin
      SetLength(Text, ttRecord.uStringLength);
      Move(pTempBuffer^, Text[1], ttRecord.uStringLength);
    end;
  end;

begin
  Result := False;

  pActBuffer := pBuffer;

  Move(pActBuffer^, ttNTHeader, sizeof(TT_NAME_TABLE_HEADER));
  inc(pActBuffer, sizeof(TT_NAME_TABLE_HEADER));

  ttNTHeader.uNRCount := SWAPWORD(ttNTHeader.uNRCount);
  ttNTHeader.uStorageOffset := SWAPWORD(ttNTHeader.uStorageOffset);

  Prio := -1;

  for Idx := 0 to ttNTHeader.uNRCount -1 do begin
    Move(pActBuffer^, ttRecord, sizeof(TT_NAME_RECORD));
    Inc(pActBuffer, sizeof(TT_NAME_RECORD));

    ttRecord.uNameID := SWAPWORD(ttRecord.uNameID);

    if ttRecord.uNameID = NameID then begin
      ttRecord.uPlatformID := SWAPWORD(ttRecord.uPlatformID);
      ttRecord.uEncodingID := SWAPWORD(ttRecord.uEncodingID);
      ttRecord.uLanguageID := SWAPWORD(ttRecord.uLanguageID);

      // highest priority
      if (ttRecord.uPlatformID = PLATFORM_ID_MICROSOFT) then begin
        // system language
        if (ttRecord.uLanguageID = languageID) then begin
          if Prio <= 7 then begin
            ExtractName;

            Prio := 7;
          end;
        end else

        // english
        if (ttRecord.uLanguageID = 1033) then begin
          if Prio <= 6 then begin
            ExtractName;

            Prio := 6;
          end;
        end else

        // all else
        if Prio <= 5 then begin
          ExtractName;

          Prio := 5;
        end;
      end else

      // apple unicode
      if (ttRecord.uPlatformID = PLATFORM_ID_APPLE_UNICODE) then begin
        ExtractName;

        Prio := 4;
      end else

      // macintosh
      if (ttRecord.uPlatformID = PLATFORM_ID_MACINTOSH) then begin
        // english
        if (ttRecord.uLanguageID = 0) then begin
          if Prio <= 3 then begin
            ExtractName;

            Prio := 3;
          end;
        end else

        // all other
        begin
          ExtractName;

          Prio := 2;
        end;
      end else

      begin
        if Prio <= 1 then begin
          ExtractName;

          Prio := 1;
        end;
      end;
    end;
  end;
end;

function GetTTFontFullNameFromStream(Stream: TStream; LanguageID: Cardinal): AnsiString;
var
  TableName: Cardinal;
  Buffer: Pointer;
  BufferSize: Integer;
begin
  TableName := MakeTTTableName('n', 'a', 'm', 'e');

  if GetTTTableData(Stream, TableName, nil, BufferSize) then begin
    GetMem(Buffer, BufferSize);
    try
      if GetTTTableData(Stream, TableName, Buffer, BufferSize) then begin
        if not GetTTString(Buffer, BufferSize, NAME_ID_FULL_NAME, LanguageID, Result) then
          if not GetTTString(Buffer, BufferSize, NAME_ID_FACE_NAME, LanguageID, Result) then
            Result := '';
      end;
    finally
      FreeMem(Buffer);
    end;
  end;
end;

function GetTTFontFullNameFromFile(Filename: AnsiString; LanguageID: Cardinal): AnsiString;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(String(Filename), fmOpenRead or fmShareDenyWrite);
  try
    GetTTFontFullNameFromStream(fs, LanguageID);
  finally
    fs.Free;
  end;
end;


(*
{ CMAP table Data
  From the TrueType Spec revision 1.66

  USHORT  Table Version #
  USHORT  Number of encoding tables  }
const
  CMAPHEADERSIZE  = sizeof(Word) * 2;
  ENCODINGSIZE    = sizeof(Word) * 2 + sizeof(Cardinal);

  // 'cmap'
  CMAP_TABLE_NAME = $70616D63;


type
  PCMAP4 = ^TCMAP4;
  TCMAP4 = packed record
    format: Word;
    length: Word;
    version: Word;
    segCountX2: Word;
    searchRange: Word;
    entrySelector: Word;
    rangeShift: Word;

    Arrays: array[1..1] of Word;
  end;


type
  PCMAPENCODING = ^TCMAPENCODING;
  TCMAPENCODING = packed record
    PlatformId: Word;
    EncodingId: Word;
    Offset: Cardinal;
  end;

  PWordArray = ^TWordArray;
  TWordArray = array [0..$FFFF] of Word;



function GetEndCountArray(pBuff: pByte): PWORD;
begin
//  Inc(pBuff, 7 * SizeOf(WORD));
  Inc(pBuff, 14);

  Result := Pointer(pBuff);
end;


function GetStartCountArray(pBuff: pByte): PWORD;
var
  segCount: Cardinal;
begin
  segCount := PCMAP4(pBuff)^.segCountX2 shr 1;

//  Inc(pBuff, 8 * SizeOf(WORD) + segCount * SizeOf(WORD));
  Inc(pBuff, 16 + segCount * 2);
  Result := Pointer(pBuff);
end;


function GetIdDeltaArray(pBuff: pByte): PWORD;
var
  segCount: Cardinal;
begin
  segCount := PCMAP4(pBuff)^.segCountX2 shr 1;

//  Inc(pBuff, 8 * SizeOf(WORD) + segCount * 2 * SizeOf(WORD));
  Inc(pBuff, 16 + segCount * 4);
  Result := Pointer(pBuff);
end;


function GetIdRangeOffsetArray(pBuff: pByte): PWORD;
var
  segCount: Cardinal;
begin
  segCount := PCMAP4(pBuff)^.segCountX2 shr 1;

//  Inc(pBuff, 8 * SizeOf(WORD) + segCount * 3 * SizeOf(WORD));
  Inc(pBuff, 16 + segCount * 6);
  Result := Pointer(pBuff);
end;


function GetGlyphIdsArray(pBuff: pByte): PWORD;
var
  segCount: Cardinal;
begin
  segCount := PCMAP4(pBuff)^.segCountX2 shr 1;

//  Inc(pBuff, 8 * SizeOf(WORD) + segCount * 4 * SizeOf(WORD));
  Inc(pBuff, 16 + segCount * 8);
  Result := Pointer(pBuff);
end;


procedure SwapArrays(pFormat4: PCMAP4);
var
  segCount: Cardinal;
  i: Cardinal;
  pGlyphId: PWord;
  pEndOfBuffer: PWord;
  pstartCount: PWord;
  pidDelta: PWord;
  pidRangeOffset: PWord;
  pendCount: PWord;
begin
  segCount       := pFormat4^.segCountX2 shr 1;
  pstartCount    := GetStartCountArray(pByte(pFormat4));
  pidDelta       := GetIdDeltaArray(pByte(pFormat4));
  pidRangeOffset := GetIdRangeOffsetArray(pByte(pFormat4));
  pendCount      := GetEndCountArray(pByte(pFormat4));

  // Swap the array elements for Intel.
  for i := 0 to segCount -1 do begin
    pWordArray(pendCount)^[i]      := SWAPWORD(pWordArray(pendCount)^[i]);
    pWordArray(pstartCount)^[i]    := SWAPWORD(pWordArray(pstartCount)^[i]);
    pWordArray(pidDelta)^[i]       := SWAPWORD(pWordArray(pidDelta)^[i]);
    pWordArray(pidRangeOffset)^[i] := SWAPWORD(pWordArray(pidRangeOffset)^[i]);
  end;

  // Swap the Glyph Id array
  pGlyphId := pWord(Cardinal(pidRangeOffset) + segCount);
  pEndOfBuffer := pWord(Cardinal(pFormat4) + pFormat4.length);

  while Cardinal(pGlyphId) < Cardinal(pEndOfBuffer) do begin
    pGlyphId^ := SWAPWORD(pGlyphId^);

    Inc(pGlyphId);
  end;
end;



function GetFontEncoding(dc: Cardinal; pEncoding: PCMAPENCODING; iEncoding: Integer): Boolean;
var
  dwResult: Cardinal;
begin
  // Get the structure data from the TrueType font
  dwResult := GetFontData (dc, CMAP_TABLE_NAME, CMAPHEADERSIZE + ENCODINGSIZE * iEncoding, pEncoding, sizeof(TCMAPENCODING));

  Result := (dwResult = sizeof(TCMAPENCODING));

  // swap the Platform Id for Intel
  pEncoding^.PlatformId := SWAPWORD(pEncoding^.PlatformId);

  // swap the Specific Id for Intel
  pEncoding^.EncodingId := SWAPWORD(pEncoding^.EncodingId);

  // swap the subtable offset for Intel
  pEncoding^.Offset := SWAPLONG(pEncoding^.Offset);
end;


function GetFontFormat4Header(dc: Cardinal; pFormat4: PCMAP4; dwOffset: Cardinal): Boolean;
var
  dwResult: Cardinal;
  i: Integer;
  pField: pWord;
begin
  Result := True;

  // Loop and Alias a writeable pointer to the field of interest
  pField := pWord(pFormat4);

  for I := 0 to 7 -1 do begin
    // Get the field from the subtable
    dwResult := GetFontData(dc, CMAP_TABLE_NAME, dwOffset + sizeof(WORD) * Cardinal(i), pField, sizeof(Word));

    // swap it to make it right for Intel.
    pField^ := SWAPWORD(pField^);

    // move on to the next
    Inc(pField);

    // accumulate our success
    Result := (dwResult = sizeof(Word)) and Result;
  end;
end;


function GetFontFormat4Subtable(dc: Cardinal; pFormat4Subtable: PCMAP4; dwOffset: Cardinal): Boolean;
var
  dwResult: Cardinal;
  length: Word;
begin
  Result := False;

  // Retrieve the header values in swapped order
  if (GetFontFormat4Header(dc, pFormat4Subtable, dwOffset)) then begin
    // Get the rest of the table
    length := pFormat4Subtable^.length - (7 * sizeof(Word));

    dwResult := GetFontData(dc, CMAP_TABLE_NAME, dwOffset + 7 * sizeof(Word), @(pFormat4Subtable^.Arrays), length);

    if (dwResult = length) then begin
      // Swamp the arrays
      SwapArrays(pFormat4Subtable);

      Result := True;
    end;
  end;
end;


function GetFontFormat4CharCount(pFormat4: PCMAP4): Word;
var
  i: Word;
  pendCount: PWord;
  pstartCount: PWord;
  idRangeOffset: PWord;

  nGlyphs: Word;

  idResult: Word;
  ch: Word;
begin
  Result := 0;

  if pFormat4 <> nil then begin
    pendCount := GetEndCountArray(pByte(pFormat4));
    pstartCount := GetStartCountArray(pByte(pFormat4));
    idRangeOffset := GetIdRangeOffsetArray(pByte(pFormat4));

    // Count the # of glyphs
    nGlyphs := 0;

    // by adding up the coverage of each segment
    for i := 0 to (pFormat4^.segCountX2 shr 1) -1 do begin

      if pWordArray(idRangeOffset)^[i] = 0 then begin
        // if per the TT spec, the idRangeOffset element is zero,
        // all of the characters in this segment exist.
        nGlyphs := nGlyphs + pWordArray(pendCount)^[i] - pWordArray(pstartCount)^[i] +1;
      end else

      begin
        // otherwise we have to test for glyph existence for
        // each character in the segment.
        for ch := pWordArray(pstartCount)^[i] to pWordArray(pendCount)^[i] do begin

          // determine if a glyph exists
          //idResult = *( idRangeOffset[i]/2 + (ch - pstartCount[i]) +  &idRangeOffset[i] );
          idResult :=
//            pWordArray(idRangeOffset)^[i] shr 1 +
            (ch - pWordArray(pstartCount)^[i]) +
            pWordArray(idRangeOffset)^[i];

          if (idResult <> 0) then
            Inc(nGlyphs);
        end;
      end;
    end;

    Result := nGlyphs;
  end;
end;




function GetTTUnicodeCoverage(dc: Cardinal; pBuffer: PCMAP4; cbSize: Cardinal; pcbNeeded: PCardinal): Boolean;
{   if cbSize is to small or zero, or if pBuffer is NULL the function
    will fail and return the required buffer size in *pcbNeeded.

    if another error occurs, the function will fail and *pcbNeeded will
    be zero.

    When the function succeeds, *pcbNeeded contains the number of bytes
    copied to pBuffer. }
var
  nEncodings: Word;
  Encoding: TCMAPENCODING;
  dwResult: Cardinal;
  i: Cardinal;
  iUnicode: Cardinal;
  Format4: TCMAP4;
  pFormat4Subtable: PCMAP4;
begin
  Result := False;
  pcbNeeded^ := 0;

  // Get the number of subtables in the CMAP table from the CMAP header
  // The # of subtables is the second USHORT in the CMAP table, per the TT Spec.
  dwResult := GetFontData(dc, CMAP_TABLE_NAME, sizeof(Word), @nEncodings, sizeof(Word));
  nEncodings := SWAPWORD(nEncodings);

  if (dwResult = sizeof(Word)) then begin
    // Get the encodings and look for a Unicode Encoding
    iUnicode := nEncodings;

    for i := 0 to nEncodings -1 do begin
      // Get the encoding entry for each encoding
      if (not GetFontEncoding(dc, @Encoding, i)) then
        Exit;

      // Take note of the Unicode encoding.
      //
      // A Unicode encoding per the TrueType specification has a
      // Platform Id of 3 and a Platform specific encoding id of 1
      // Note that Symbol fonts are supposed to have a Platform Id of 3
      // and a specific id of 0. If the TrueType spec. suggestions were
      // followed then the Symbol font's Format 4 encoding could also
      // be considered Unicode because the mapping would be in the
      // Private Use Area of Unicode. We assume this here and allow
      // Symbol fonts to be interpreted. If they do not contain a
      // Format 4, we bail later. If they do not have a Unicode
      // character mapping, we'll get wrong results.
      // Code could infer from the coverage whether 3-0 fonts are
      // Unicode or not by examining the segments for placement within
      // the Private Use Area Subrange.
      if ((Encoding.PlatformId = 3) and (Encoding.EncodingId in [0, 1])) then
        iUnicode := i;       // Set the index to the Unicode encoding
    end;

    // index out of range means failure to find a Unicode mapping
    if (iUnicode >= nEncodings) then
      Exit;

    // Get the header entries(first 7 USHORTs) for the Unicode encoding.
    if (not GetFontFormat4Header (dc, @Format4, Encoding.Offset)) then
      Exit;

    // Check to see if we retrieved a Format 4 table
    if (Format4.format <> 4) then
      Exit;

    // Figure buffer size and tell caller if buffer to small
    pcbNeeded^ := Format4.length;
    if (pcbNeeded^ > cbSize) or (pBuffer = nil) then
      Exit;

    // allocate a full working buffer
    GetMem(pFormat4Subtable, Format4.length);
    try
      if (pFormat4Subtable = nil) then
        Exit;

      // get the entire subtable
      if (not GetFontFormat4Subtable(dc, pFormat4Subtable, Encoding.Offset)) then
        Exit;

      // Copy the retrieved table into the buffer
      Move(pFormat4Subtable^, pBuffer^, pFormat4Subtable^.length);
    finally
      FreeMem(pFormat4Subtable);
    end;

    Result := True;
  end;
end;


function FindFormat4Segment(pTable: PCMAP4; ch: Word; piSeg: PWord): Boolean;
{   if the Unicode character ch is not contained in one of the
    segments the function returns FALSE.

    if the Unicode character ch is found in a segment, the index
    of the segment is placed in*piSeg and the function returns
    TRUE. }
var
  i: Word;
  segCount: Word;
  pendCount: PWord;
  pstartCount: PWord;
begin
  Result := False;

  segCount := pTable^.segCountX2 shr 1;
  pendCount := GetEndCountArray(pbyte(pTable));
  pstartCount := GetStartCountArray(pByte(pTable));

  // Find segment that could contain the Unicode character code
  i := 0;
  while ((i < segCount) and (pWordArray(pendCount)^[i] < ch)) do
    inc(i);

  // We looked in them all, ch not there
  if (i >= segCount) then
    Exit;

  // character code not within the range of the segment
  if (pWordArray(pstartCount)^[i] > ch) then
    Exit;

  // this segment contains the character code
  piSeg^ := i;

  Result := True;
end;


function GetTTUnicodeCharCount(dc: Cardinal): Word;
{   Returns the number of Unicode character glyphs that
    are in the TrueType font that is selected into the hdc. }
var
  pUnicodeCMapTable: PCMAP4;
  dwSize: Cardinal;
begin
  Result := 0;

  // Get the Unicode CMAP table from the TT font
  GetTTUnicodeCoverage(dc, nil, 0, @dwSize);

  GetMem(pUnicodeCMapTable, dwSize);
  try
    if (GetTTUnicodeCoverage(dc, pUnicodeCMapTable, dwSize, @dwSize)) then 
      Result := GetFontFormat4CharCount(pUnicodeCMapTable);
  finally
    FreeMem(pUnicodeCMapTable);
  end;
end;


function GetTTUnicodeGlyphIndex(dc: Cardinal; ch: Word): Word;
{   When the TrueType font contains a glyph for ch, the
    function returns the glyph index for that character.

    If an error occurs, or there is no glyph for ch, the
    function will return the missing glyph index of zero.  }
var
  pUnicodeCMapTable: PCMAP4;
  dwSize: Cardinal;
  iSegment: Word;
  idRangeOffset: PWordArray;
  idDelta: PWordArray;
  startCount: PWordArray;
//  endCount: PWordArray;
  glyphIdArray: PWordArray;
  GlyphIndex: Word;

//  Temp: PByte;
  idResult: Word;
begin
//  Result := 0;
  GlyphIndex := 0;     // Initialize to missing glyph

  // How big a buffer do we need for Unicode CMAP?
  GetTTUnicodeCoverage(dc, nil, 0, @dwSize);

  GetMem(pUnicodeCMapTable, dwSize);
  try
    if (GetTTUnicodeCoverage(dc, pUnicodeCMapTable, dwSize, @dwSize)) then begin
      // Find the cmap segment that has the character code.
      if (FindFormat4Segment(pUnicodeCMapTable, ch, @iSegment)) then begin
        // Get pointers to the cmap data
        idRangeOffset := pWordArray(GetIdRangeOffsetArray(pByte(pUnicodeCMapTable)));
        idDelta       := pWordArray(GetIdDeltaArray(pByte(pUnicodeCMapTable)));
        startCount    := pWordArray(GetStartCountArray(pByte(pUnicodeCMapTable)));
//        endCount      := pWordArray(GetEndCountArray(pByte(pUnicodeCMapTable)));

        // Per TT spec, if the RangeOffset is zero,
        if (idRangeOffset^[iSegment] = 0) then begin
          // calculate the glyph index directly
          GlyphIndex := (idDelta^[iSegment] + ch) mod 65536;
        end else

        begin
          // otherwise, use the glyph id array to get the index
//        idResult = *(
//            idRangeOffset[iSegment]/2 +
//            (ch - startCount[iSegment]) +
//            &idRangeOffset[iSegment]
//            );  // indexing equation from TT spec

//          glyphIdArray := pWordArray(GetGlyphIdsArray(pByte(pUnicodeCMapTable)));
//          idResult := glyphIdArray^[(idRangeOffset^[iSegment] div 2) + iSegment - (pUnicodeCMapTable^.segCountX2 div 2) + (ch - startCount^[iSegment])];

// funktioniert so halb -- anfang
          glyphIdArray := pWordArray(
            Cardinal(idRangeOffset) + iSegment + 
            idRangeOffset^[iSegment]
          );

(* So halb
          glyphIdArray := pWordArray(
            Cardinal(@(idRangeOffset^[iSegment])) +
            idRangeOffset^[iSegment]
          );
* )

//          glyphIdArray := pWordArray(
//            Cardinal(@(idRangeOffset^[iSegment])) +
//            idRangeOffset^[iSegment] div 2
//          );

          if (pWord(@(idRangeOffset^[pUnicodeCMapTable^.segCountX2 shr 1]))^ = 0) or
             (pWord(@(idRangeOffset^[pUnicodeCMapTable^.segCountX2 shr 1]))^ = 256) then
            Inc(pWord(glyphIdArray));

          idResult := glyphIdArray^[ch - startCount^[iSegment]];
// funktioniert so halb -- ende


//          glyphIdArray := pWordArray(Cardinal(idRangeOffset) + idRangeOffset^[iSegment]);

          if (idResult > 0) then begin
            // Per TT spec, nonzero means there is a glyph
            GlyphIndex := (PWordArray(idDelta)^[iSegment] + idResult) mod 65536
//            GlyphIndex := (idDelta^[iSegment] + idResult) mod 65536;
          end else
            // otherwise, return the missing glyph
            GlyphIndex := 0;
        end;
      end;
    end;

    Result := GlyphIndex;
  finally
    FreeMem(pUnicodeCMapTable);
  end;
end;
*)

end.
