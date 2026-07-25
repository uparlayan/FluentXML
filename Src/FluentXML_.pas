{-------------------------------------------------------------------------------
-  Author      : Uğur PARLAYAN                                                 -
-  Email       : ugurparlayan@gmail.com                                        -
-  Class Name  : TFluentXML Generator.                                         -
-  Description : This unit demonstrates how we can produce an XML document     -
-                in Object Pascal (Delphi) with a simple way of using          -
-                the Fluent Design pattern and is offered to                   -
-                community service for this purpose.                           -
-  Create Date : 2017-09-12                                                    -
-  Update Date : 2020-01-06                                                    -
-  Update Date : 2020-08-16                                                    -
-  Update Date : 2026-07-26                                                    -
-  License     : GPL-3.0                                                       -
-  Copyright (C) 2017 Uğur PARLAYAN                                            -
-------------------------------------------------------------------------------}
unit FluentXML_;

interface

uses
    System.SysUtils
  , System.StrUtils
  , System.Variants
  , System.Classes
  , Data.DB
  ;

type
  /// <summary>
  /// XML dosyası üretmek için kullanılır. Node'lar fluent mantığına uygun olarak kuyruk şeklinde ard arda eklenebilir ve Tür, kendi kendini parametre olarak kullanabilir.
  /// </summary>
  TFluentXML = class
    const
      /// <summary>
      ///  TAB (#9) karakteri ile yer değiştirecek olan sabittir. Hem kodlamada hem de XML Biçimlendirmede kullanılır.
      /// </summary>
      /// <remarks>
      ///  FormatXML işlevinde bu karakter #9 veya #32#32'ye dahili olarak dönüştürülür.
      /// </remarks>
      Tab = #1;
    type
      TVarArray       = array of Variant;
      TVarArrayHelper = record helper for TVarArray
        function Split(aDelimiter: String): String;
      end;
      TEncodingHelper = class Helper for TEncoding
        function AsEncoderName: String;
      end;
    private
      _Version    : Double;
      _Encoding   : TEncoding;
      _NameSpace  : string;
      _StyleSheet : string;
      _Source     : String;
      _Root       : String;
      class var FXMLFormatSettings: TFormatSettings;
    strict private
      function _if(aKosul: Boolean; aTrue, aFalse: String): String; overload;
      function _f(const aFormat: string; const aArgs: array of const): string;
      function _NS: String;
      function XMLEscape(const aText: String): String;
    public
      /// <summary>
      ///  String Export amacıyla kullanılır. Tür String olduğu için fluent akışı bozulur. O nedenle aşırı yüklenmiş diğer fonksiyonu kullanın.
      /// </summary>
      function AsString: String; overload;
      /// <summary>
      ///  String Export amacıyla kullanılır. Parametre olarak aldığı değişkene bünyesinde tuttuğu XML kaynak kodunu aktarır.
      /// </summary>
      function AsString(out aStringVariable: String): TFluentXML; overload;
      function Root(aName: String): TFluentXML;
      function Version(aValue: Double): TFluentXML;
      function Encoding(aValue: TEncoding): TFluentXML;
      function NameSpace(aValue: String): TFluentXML;
      function StyleSheet(aType, aHref: String): TFluentXML;
      function Add(aNode: TFluentXML): TFluentXML; overload;
      function Add(aNodes: Array of TFluentXML): TFluentXML; overload;
      function Add(aNode: string): TFluentXML; overload;
      function Add(aNode: string; aValue: Variant): TFluentXML; overload;
      function Add(aNode: string; aSubNode: TFluentXML): TFluentXML; overload;
      function Add(aNode: string; aAttributes: TVarArray): TFluentXML; overload;
      function Add(aNode: string; aAttributes: TVarArray; aValue: Variant): TFluentXML; overload;
      function Add(aNode: string; aAttributes: TVarArray; aSubNode: TFluentXML): TFluentXML; overload;
      function Add(aNodeName: string; aDataSet: TDataSet): TFluentXML; overload;
      function Add(aNodeName, aRowName: string; aDataSet: TDataSet): TFluentXML; overload;
      function SaveToFile(aFileName: TFileName): TFluentXML;
      function FormatXml: TFluentXML;
      class function New(aVersion: Double; aEncoding: TEncoding): TFluentXML;
  end;
  function New: TFluentXML; overload;
  function New(aNameSpace: String): TFluentXML; overload;
  function New(aEncoding: TEncoding): TFluentXML; overload;
  function New(aNodes: Array of TFluentXML): TFluentXML; overload;
  function XML: TFluentXML; overload;
  function XML(aNameSpace: String): TFluentXML; overload;
  function XML(aEncoding: TEncoding): TFluentXML; overload;

implementation

function New: TFluentXML;
begin
  Result := TFluentXML.Create;
end;

function New(aNameSpace: String): TFluentXML; overload;
begin
  Result := TFluentXML.Create;
  Result._NameSpace := aNameSpace.Trim;
end;

function New(aEncoding: TEncoding): TFluentXML; overload;
begin
  Result := TFluentXML.Create;
  Result._Encoding := aEncoding;
end;

function New(aNodes: Array of TFluentXML): TFluentXML; overload;
var
  X: TFluentXML;
  I: Integer;
begin
  Result := TFluentXML.Create;
  for X in aNodes do Result._Source := Result._Source + X.AsString;
  for I := High(aNodes) downto Low(aNodes) do FreeAndNil(aNodes[I]);
end;

function XML: TFluentXML;
begin
  Result := New;
end;

function XML(aNameSpace: String): TFluentXML; overload;
begin
  Result := New(aNameSpace);
end;

function XML(aEncoding: TEncoding): TFluentXML; overload;
begin
  Result := New(aEncoding);
end;

function TFluentXML.AsString(out aStringVariable: String): TFluentXML;
begin
  aStringVariable := Self.AsString;
  Result := Self;
end;

function TFluentXML.AsString: String;
var
  Tmp: String;
begin
  Tmp := _Encoding.AsEncoderName;
  if (Pos( '<?xml',_Source, 1) <= 0) then begin
      _Source := _if( ((_Version <> 0) or (Tmp.IsEmpty = False))
                    , _f ( '<?xml%s%s?>'#13#10,
                         [ _if(_Version <> 0, _f(' version="%s"', [ FormatFloat('0.0', _Version, FXMLFormatSettings)]), '')
                         , _if(Tmp.IsEmpty = False, _f(' encoding="%s"', [Tmp]), '')
                         ])
                    , '')
               + _if( _StyleSheet.Trim.IsEmpty, '', _StyleSheet + #13#10)
               + _Source
               { // Bu kısım FormatXml işlevine taşındı
               + _if( _Root.Trim.IsEmpty
                    , _Source
                    , _f('<%0:s>'#13#10'%1:s'#13#10'</%0:s>'#13#10,
                          [ _if( _NameSpace.Trim.IsEmpty = true
                               , _Root
                               , _NS + _Root
                               )
                          , _Source
                          ])
                    )
               }
               ;
  end;
  Result := _Source.Trim;
end;

function TFluentXML.Version(aValue: Double): TFluentXML;
begin
  _Version := aValue;
  Result := Self;
end;

class function TFluentXML.New(aVersion: Double; aEncoding: TEncoding): TFluentXML;
begin
  Result := TFluentXML.Create;
  Result.Version(aVersion);
  Result.Encoding(aEncoding);
end;

function TFluentXML.Root(aName: String): TFluentXML;
begin
  _Root := aName;
  Result := Self;
end;

function TFluentXML.Encoding(aValue: TEncoding): TFluentXML;
begin
  _Encoding := aValue;
  Result := Self;
end;

function TFluentXML.FormatXml: TFluentXML;
var
  I, B, TabCount, Cap: Integer;
  O, X, N: Char;
  Ek: string;
  TagInside, Tirnak, cData: Boolean;
  Buffer, ResultStr: string;
  DestP: PChar;

  procedure AppendChar(C: Char);
  var
    Used: Integer;
  begin
    Used := DestP - PChar(Buffer);
    if Used + 1 > Cap then begin
        Cap := Cap * 2 + 1024;
        SetLength(Buffer, Cap);
        DestP := PChar(Buffer) + Used;
    end;
    DestP^ := C;
    Inc(DestP);
  end;

  procedure AppendString(const S: string);
  var
    Len, Used: Integer;
  begin
    Len := Length(S);
    if Len = 0 then Exit;
    Used := DestP - PChar(Buffer);
    if Used + Len > Cap then begin
        Cap := (Cap + Len) * 2 + 1024;
        SetLength(Buffer, Cap);
        DestP := PChar(Buffer) + Used;
    end;
    Move(PChar(S)^, DestP^, Len * SizeOf(Char));
    Inc(DestP, Len);
  end;

begin
  if (_Root.Trim.IsEmpty = False) then _Source := _f('<%0:s%1:s>%2:s</%0:s%1:s>', [_NS, _Root.Trim, _Source]);
  B := Length(_Source);
  if B = 0 then Exit(Self);

  Cap := B * 2 + 1024;
  SetLength(Buffer, Cap);
  DestP := PChar(Buffer);

  O := #0;
  X := #0;
  N := #0;
  TabCount    := 1;
  TagInside   := (_Source[1] = '<');
  Tirnak      := FALSE;
  cData       := FALSE;

  for I := 1 to B do begin
      Ek := '';
      O := X;
      X := _Source[I];
      if (I < B) then N := _Source[I + 1] else N := #0;
      if (I < B - 2) then begin
          if (X = '<') and (N = '!') then cData := True;
          if (O = ']') and (X = '>') then cData := FALSE;
      end;
      if (X = '"') then Tirnak := Not Tirnak;
      if ((Tirnak = FALSE) and (cData = False)) then begin
          case TagInside of
               FALSE: Begin
                        if (X = '<') then begin
                            TagInside := True;
                            Inc(TabCount);
                            if (N = '/')
                            or (N = '!') then begin
                                Dec(TabCount, 1);
                            end;
                        end;
                      End;
               TRUE : begin
                        if (O = '<') and (X = '/')  then Dec(TabCount, 1);
                        if (X = '>') then begin
                            TagInside := False;
                            if (O = '/') then begin
                                Dec(TabCount);
                            end;
                            if (N = '<') then begin
                                Ek := #13#10 + DupeString(Tab, TabCount);
                                if (I < B - 2) then begin
                                    if (_Source[I + 2] = '!') then Ek := '';
                                end;
                            end;
                        end;
                      end;
          end;
      end;
      AppendChar(X);
      if Ek <> '' then AppendString(Ek);
  end;

  SetLength(Buffer, DestP - PChar(Buffer));
  ResultStr := Buffer.Trim;
  B := Length(ResultStr);

  Cap := B + 1024;
  SetLength(Buffer, Cap);
  DestP := PChar(Buffer);

  for I := 1 to B do begin
      O := ResultStr[I];
      if (I < B - 2) then begin
          X := ResultStr[I + 1];
          N := ResultStr[I + 2];
      end else begin
          X := #0;
          N := #0;
      end;
      if NOT ( (O = Tab) and (X = '<') and (N = '/') ) then AppendChar(O);
  end;

  SetLength(Buffer, DestP - PChar(Buffer));
  _Source := StringReplace(Buffer, Tab, #32#32, [rfReplaceAll]);
  Result := Self;
end;

function TFluentXML.NameSpace(aValue: String): TFluentXML;
begin
  _NameSpace := aValue.Trim;
  Result := Self;
end;

function TFluentXML.Add(aNode: string): TFluentXML;
begin
  _Source := _Source + _f('<%s/>', [aNode.Trim]) ;
  Result := Self;
end;

function TFluentXML.Add(aNode: string; aValue: Variant): TFluentXML;
begin
  _Source := _Source + _f('<%0:s%1:s>%2:s</%0:s%1:s>', [_NS, aNode.Trim, XMLEscape(VarToStr(aValue).Trim)]) ;
  Result := Self;
end;

function TFluentXML.Add(aNode: string; aSubNode: TFluentXML): TFluentXML;
begin
  if (Assigned(aSubNode) = TRUE) then begin
      _Source := _Source + _f('<%0:s%1:s>%2:s</%0:s%1:s>', [_NS, aNode.Trim, aSubNode.AsString]);
      FreeAndNil(aSubNode);
  end;
  Result := Self;
end;

function TFluentXML.Add(aNode: TFluentXML): TFluentXML;
begin
  if (Assigned(aNode) = TRUE) then begin
      _Source := _Source + aNode.AsString;
      FreeAndNil(aNode);
  end;
  Result := Self;
end;

function TFluentXML.Add(aNodes: Array of TFluentXML): TFluentXML;
var
  X: TFluentXML;
  I: Integer;
begin
  for X in aNodes do begin
      _Source := _Source + X.AsString;
  end;
  for I := High(aNodes) downto Low(aNodes) do FreeAndNil(aNodes[I]);
  Result := Self;
end;

function TFluentXML.Add(aNode: string; aAttributes: TVarArray; aValue: Variant): TFluentXML;
var
  Tmp: String;
begin
  Tmp := aAttributes.Split(' ').Trim;
  _Source := _Source
           + _f('<%0:s%1:s%2:s>%3:s</%0:s%1:s>', [_NS, aNode.Trim, _if(Tmp.IsEmpty = True, '', ' ' + Tmp), XMLEscape(VarToStr(aValue).Trim)]) ;
  Result := Self;
end;

function TFluentXML.Add(aNode: string; aAttributes: TVarArray; aSubNode: TFluentXML): TFluentXML;
var
  Tmp: String;
begin
  if (Assigned(aSubNode) = TRUE) then begin
      Tmp := aAttributes.Split(' ').Trim;
      _Source := _Source
               + _f('<%0:s%1:s%2:s>%3:s</%0:s%1:s>', [_NS, aNode.Trim, _if(Tmp.IsEmpty = True, '', ' ' + Tmp), aSubNode.AsString]);
      FreeAndNil(aSubNode);
  end;
  Result := Self;
end;

function TFluentXML.Add(aNodeName: string; aDataSet: TDataSet): TFluentXML;
begin
  Result := Add(aNodeName, 'Row', aDataSet);
end;

function TFluentXML.Add(aNodeName, aRowName: string; aDataSet: TDataSet): TFluentXML;
var
  ContainerNode, RowNode: TFluentXML;
  Field: TField;
  ActualRowName: string;
begin
  if (Assigned(aDataSet) = False) or aDataSet.IsEmpty then Exit(Self);

  if aRowName.Trim.IsEmpty then
    ActualRowName := 'Row'
  else
    ActualRowName := aRowName.Trim;

  if aNodeName.Trim.IsEmpty then
    ContainerNode := Self
  else
    ContainerNode := TFluentXML.Create;

  aDataSet.DisableControls;
  try
    aDataSet.First;
    while not aDataSet.Eof do begin
        RowNode := TFluentXML.Create;
        for Field in aDataSet.Fields do begin
            if not Field.IsNull then
              RowNode.Add(Field.FieldName, Field.AsString);
        end;

        ContainerNode.Add(ActualRowName, RowNode);
        aDataSet.Next;
    end;
  finally
    aDataSet.EnableControls;
  end;

  if (aNodeName.Trim.IsEmpty = False) then
    Self.Add(aNodeName.Trim, ContainerNode);

  Result := Self;
end;

function TFluentXML.Add(aNode: string; aAttributes: TVarArray): TFluentXML;
var
  Tmp: String;
begin
  Tmp := aAttributes.Split(' ');
  _Source := _Source + _f('<%0:s%1:s%2:s/>', [_NS, aNode.Trim, _if(Tmp.IsEmpty = True, '', ' ' + Tmp)]) ;
  Result := Self;
end;

function TFluentXML._f(const aFormat: string; const aArgs: array of const): string;
begin
  Result := Format(aFormat, aArgs);
end;

function TFluentXML._if(aKosul: Boolean; aTrue, aFalse: String): String;
begin
  if (aKosul = TRUE) then Result := aTrue else Result := aFalse;
end;

function TFluentXML._NS: String;
begin
  Result := _if( (_NameSpace.Trim.IsEmpty = True), '', _NameSpace.Trim+':');
end;

function TFluentXML.XMLEscape(const aText: String): String;
begin
  Result := aText;
  if Result.IsEmpty then Exit;
  Result := StringReplace(Result, '&',  '&amp;',  [rfReplaceAll]);
  Result := StringReplace(Result, '<',  '&lt;',   [rfReplaceAll]);
  Result := StringReplace(Result, '>',  '&gt;',   [rfReplaceAll]);
  Result := StringReplace(Result, '"',  '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&apos;', [rfReplaceAll]);
end;

function TFluentXML.SaveToFile(aFileName: TFileName): TFluentXML;
var
  Dosya : TStreamWriter;
  TargetDir : String;
begin
  try
    TargetDir := ExtractFileDir(aFileName);
    if TargetDir.IsEmpty or DirectoryExists(TargetDir, True) then begin
        try
          Dosya := TStreamWriter.Create(aFileName, False, TEncoding.UTF8);
          Dosya.Write(Self.AsString);
          Dosya.Close;
        finally
          FreeAndNil(Dosya);
        end;
    end else begin
        raise Exception.Create('Directory not found: ' + TargetDir);
    end;
  finally
    Result := Self;
  end;
end;

function TFluentXML.StyleSheet(aType, aHref: String): TFluentXML;
begin
  _StyleSheet := _f('<?xml-stylesheet type="%s" href="%s"?>', [aType.Trim, aHref.Trim]);
  Result := Self;
end;

{ TVarArrayHelper }

function TFluentXML.TVarArrayHelper.Split(aDelimiter: String): String;
var
  I: Integer;
begin
  for I := Low(Self) to High(Self)
  do if  (I < High(Self) )
     then Result := Result + VarToStrDef(Self[I], '').Trim + aDelimiter
     else Result := Result + VarToStrDef(Self[I], '').Trim;
end;

{ TEncodingHelper }

function TFluentXML.TEncodingHelper.AsEncoderName: String;
begin
  {-- Sources ------------------------------------------------------------------------}
  { https://docs.microsoft.com/en-us/dotnet/standard/base-types/character-encoding    }
  { http://www.iana.org/assignments/character-sets/character-sets.xhtml               }
  {-----------------------------------------------------------------------------------}
  if (Self = TEncoding.ANSI)             then Result := 'ANSI'         else
  if (Self = TEncoding.ASCII)            then Result := 'ASCII'        else
  if (Self = TEncoding.UTF7)             then Result := 'UTF-7'        else
  if (Self = TEncoding.UTF8)             then Result := 'UTF-8'        else
  if (Self = TEncoding.Unicode)          then Result := 'UTF-16'       else
  if (Self = TEncoding.BigEndianUnicode) then Result := 'UTF-16BE'     else
  if (Self = TEncoding.Default)          then Result := 'Windows-1254' else
  Result := '';
end;

initialization
  TFluentXML.FXMLFormatSettings := TFormatSettings.Invariant;

end.
