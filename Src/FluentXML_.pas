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
    strict private
      function _if(aKosul: Boolean; aTrue, aFalse: String): String; overload;
      function _f(const aFormat: string; const Args: array of const): string;
      function _NS: String;
    public
      /// <summary>
      ///  String Export amacıyla kullanılır. Tür String olduğu için fluent akışı bozulur. O nedenle aşırı yüklenmiş diğer fonksiyonu kullanın.
      /// </summary>
      function AsString: String; overload;
      /// <summary>
      ///  String Export amacıyla kullanılır. Parametre olarak aldığı değişkene bünyesinde tuttuğu XML kaynak kodunu aktarır.
      /// </summary>
      function AsString(out aStringVariable: String): TFluentXML; overload;
      function Root(Name: String): TFluentXML;
      function Version(Value: Double): TFluentXML;
      function Encoding(Value: TEncoding): TFluentXML;
      function NameSpace(Value: String): TFluentXML;
      function StyleSheet(aType, aHref: String): TFluentXML;
      function Add(aNode: TFluentXML): TFluentXML; overload;
      function Add(aNodes: Array of TFluentXML): TFluentXML; overload;
      function Add(aNode: string): TFluentXML; overload;
      function Add(aNode: string; aValue: Variant): TFluentXML; overload;
      function Add(aNode: string; aSubNode: TFluentXML): TFluentXML; overload;
      function Add(aNode: string; aAttributes: TVarArray): TFluentXML; overload;
      function Add(aNode: string; aAttributes: TVarArray; aValue: Variant): TFluentXML; overload;
      function Add(aNode: string; aAttributes: TVarArray; aSubNode: TFluentXML): TFluentXML; overload;
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
  FS: TFormatSettings;
begin
  FS := FormatSettings;
  FormatSettings.DecimalSeparator := '.';
  Tmp := _Encoding.AsEncoderName;
  if (Pos( '<?xml',_Source, 1) <= 0) then begin
      _Source := _if( ((_Version <> 0) or (Tmp.IsEmpty = False))
                    , _f ( '<?xml%s%s?>'#13#10,
                         [ _if(_Version <> 0, _f(' version="%s"', [ formatfloat('0.0',_Version)]), '')
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
  FormatSettings := FS;
  Result := _Source.Trim;
end;

function TFluentXML.Version(Value: Double): TFluentXML;
begin
  _Version := Value;
  Result := Self;
end;

class function TFluentXML.New(aVersion: Double; aEncoding: TEncoding): TFluentXML;
begin
  Result := TFluentXML.Create;
  Result.Version(aVersion);
  Result.Encoding(aEncoding);
end;

function TFluentXML.Root(Name: String): TFluentXML;
begin
  _Root := Name;
  Result := Self;
end;

function TFluentXML.Encoding(Value: TEncoding): TFluentXML;
begin
  _Encoding := Value;
  Result := Self;
end;

function TFluentXML.FormatXml: TFluentXML;
var
  I           : Integer;     //  Indis
  B           : Integer;     //  Len / Size...
  T           : string;
  O           : Char;        //  önceki
  X           : Char;        //  şimdiki
  N           : Char;        //  sonraki
  Ek          : string;
  TabCount    : Integer;
  TagInside   : Boolean;
  IsStartTag  : Boolean;
  Tirnak      : Boolean;
  cData       : Boolean;
begin
  if (_Root.Trim.IsEmpty = False) then _Source := _f('<%0:s%1:s>%2:s</%0:s%1:s>', [_NS, _Root.Trim, _Source{, _if(_NS.Trim.IsEmpty, '', ':')}]);
  B := Length(_Source);
  O := #0;
  X := #0;
  N := #0;
  TabCount    := 1;
  TagInside   := (_Source[1] = '<');
  IsStartTag  := TagInside;
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
                            IsStartTag := True;
                            if (N = '/')
                            or (N = '!') then begin
                                IsStartTag := FALSE;
                                Dec(TabCount, 1); { </ } { veya } { <! }
                            end;
                        end;
                      End;
               TRUE : begin
                        if (O = '<') and (X = '/')  then Dec(TabCount, 1);
                        if (X = '>') then begin
                            TagInside := False;
                            if (O = '/') then begin
                                Dec(TabCount); { /> }
                                IsStartTag := FALSE; { bu da sadece tagın göründüğü modeldir, verisiz, sadece tagın adı olur. örneğin <tag/> gibi...}
                            end;
                            if (N = '<') then begin          { >< }
                                Ek := #13#10 + DupeString(Tab, TabCount);// + '{' + TabCount.ToString + '}';
                                if (I < B - 2) then begin
                                    if (_Source[I+2] = '!') then Ek := ''; { <! }
                                end;
                            end;
                        end;
                      end;
          end;
      end;
      T := T + X + Ek;
  end;
  B := T.Trim.Length;
  _Source := T.Trim;
  T := '';
  for I := 1 to B do begin
      O := _Source[I];
      if (I < B - 2) then begin
          X := _Source[I+1];
          N := _Source[I+2];
      end else begin
          X := #0;
          N := #0;
      end;
      if NOT ( (O = Tab) and (X = '<') and (N = '/') ) then T := T + O;
  end;
  _Source := StringReplace(T, Tab, #32#32, [rfReplaceAll]);
  Result := Self;
end;

function TFluentXML.NameSpace(Value: String): TFluentXML;
begin
  _NameSpace := Value.Trim;
  Result := Self;
end;

function TFluentXML.Add(aNode: string): TFluentXML;
begin
  _Source := _Source + _f('<%s/>', [aNode.Trim]) ;
  Result := Self;
end;

function TFluentXML.Add(aNode: string; aValue: Variant): TFluentXML;
begin
  _Source := _Source + _f('<%0:s%1:s>%2:s</%0:s%1:s>', [_NS, aNode.Trim, VarToStr(aValue).Trim]) ;
  Result := Self;
end;

function TFluentXML.Add(aNode: string; aSubNode: TFluentXML): TFluentXML;
var
  Tmp: Variant;
begin
  if (Assigned(aSubNode) = TRUE) then begin
      Tmp := aSubNode.AsString;
      FreeAndNil(aSubNode);
      Result := Self.Add(aNode, Tmp);
  end else begin
      Result := Self;
  end;
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
           + _f('<%0:s%1:s%2:s>%3:s</%0:s%1:s>', [_NS, aNode.Trim, _if(Tmp.IsEmpty = True, '', ' ' + Tmp), VarToStr(aValue).Trim]) ;
  Result := Self;
end;

function TFluentXML.Add(aNode: string; aAttributes: TVarArray; aSubNode: TFluentXML): TFluentXML;
var
  Tmp: Variant;
begin
  if (Assigned(aSubNode) = TRUE) then begin
      Tmp := aSubNode.AsString;
      FreeAndNil(aSubNode);
      Result := Self.Add(aNode, aAttributes, Tmp);
  end else begin
      Result := Self;
  end;
end;

function TFluentXML.Add(aNode: string; aAttributes: TVarArray): TFluentXML;
var
  Tmp: String;
begin
  Tmp := aAttributes.Split(' ');
  _Source := _Source + _f('<%0:s%1:s%2:s/>', [_NS, aNode.Trim, _if(Tmp.IsEmpty = True, '', ' ' + Tmp)]) ;
  Result := Self;
end;

function TFluentXML._f(const aFormat: string; const Args: array of const): string;
begin
  Result := Format(aFormat, Args);
end;

function TFluentXML._if(aKosul: Boolean; aTrue, aFalse: String): String;
begin
  if (aKosul = TRUE) then Result := aTrue else Result := aFalse;
end;

function TFluentXML._NS: String;
begin
  Result := _if( (_NameSpace.Trim.IsEmpty = True), '', _NameSpace.Trim+':');
end;

function TFluentXML.SaveToFile(aFileName: TFileName): TFluentXML;
var
  Dosya : TStreamWriter;
begin
  try
    if (directoryExists(ExtractFileDir(aFileName), True) = TRUE) then begin
        try
          Dosya := TStreamWriter.Create(aFileName, False, TEncoding.UTF8);
          Dosya.Write(Self.AsString);
          Dosya.Close;
        finally
          FreeAndNil(Dosya);
        end;
    end else begin
        raise Exception.Create('Directory not found');
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

end.
