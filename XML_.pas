{*******************************************************}
{                                                       }
{       Fluent XML Class                                }
{                                                       }
{       Copyright © 2017 Uður PARLAYAN                  }
{                        ugurparlayan@gmail.com         }
{                                                       }
{*******************************************************}

unit XML_;

interface

uses
  System.SysUtils, System.StrUtils, System.Variants, System.Classes, Vcl.Dialogs;

type
  TVarArray = array of Variant;
  TVarArrayHelper = record helper for TVarArray
    public
      function Ayracli(aAyrac: String): String;
  end;
  TEncodingHelper = class Helper for TEncoding
    public
      function AsEncoderName: String;
  end;
  TXML2 = class
    private
      _Version    : Double;
      _Encoding   : TEncoding;
      _NameSpace  : string;
      _Source     : String;
    strict private
      function _if(aKosul: Boolean; aTrue, aFalse: String): String;
      function _f(const aFormat: string; const Args: array of const): string;
      function _NS: String;
    public
      function AsString: String; // Bu noktada zincir kýrýlýr...
      function Version(Value: Double): TXML2;
      function Encoding(Value: TEncoding): TXML2;
      function NameSpace(Value: String): TXML2;
      function Add(aNode: string): TXML2; overload;
      function Add(aNode: string; aValue: Variant): TXML2; overload;
      function Add(aNode: string; aSubNode: TXML2): TXML2; overload;
      function Add(aNode: string; aAttributes: TVarArray): TXML2; overload;
      function Add(aNode: string; aAttributes: TVarArray; aValue: Variant): TXML2; overload;
      function Add(aNode: string; aAttributes: TVarArray; aSubNode: TXML2): TXML2; overload;
      function SaveToFile(aFileName: TFileName): TXML2;
      class function Yeni(aVersion: Double; aEncoding: TEncoding): TXML2;
  end;
  function NewXML: TXML2;

implementation

function NewXML: TXML2;
begin
  Result := TXML2.Create;
end;

{ TXML2 }

function TXML2.AsString: String;
var
  Tmp: String;
  FS: TFormatSettings;
begin
  FS := FormatSettings;
  FormatSettings.DecimalSeparator := '.';
  Tmp := _Encoding.AsEncoderName;
  if (Pos( '<?xml',_Source,1) <= 0) then begin
      _Source := _if( ((_Version <> 0) or (Tmp.IsEmpty = False))
                    , _f ( '<?xml%s%s?>',
                         [ _if(_Version <> 0, _f(' version="%s"', [ formatfloat('0.0',_Version)]), '')
                         , _if(Tmp.IsEmpty = False, _f(' encoding="%s"', [Tmp]), '')
                         ])
                    , '')
               + _Source
               ;
  end;
  FormatSettings := FS;
  Result := StringReplace(_Source, '><', '>'#13#10'<', [rfReplaceAll, rfIgnoreCase]); // CDATA içinde geçerse sýkýntý olabilir...
end;

function TXML2.Version(Value: Double): TXML2;
begin
  _Version := Value;
  Result := Self;
end;

class function TXML2.Yeni(aVersion: Double; aEncoding: TEncoding): TXML2;
begin
  Result := TXML2.Create;
  Result.Version(aVersion);
  Result.Encoding(aEncoding);
end;

function TXML2.Encoding(Value: TEncoding): TXML2;
begin
  _Encoding := Value;
  Result := Self;
end;

function TXML2.NameSpace(Value: String): TXML2;
begin
  _NameSpace := Value.Trim;
  Result := Self;
end;

function TXML2.Add(aNode: string): TXML2;
begin
  _Source := _Source + _f('<%s/>', [aNode.Trim]) ;
  Result := Self;
end;

function TXML2.Add(aNode: string; aValue: Variant): TXML2;
begin
  _Source := _Source + _f('<%0:s%1:s>%2:s</%0:s%1:s>', [_NS, aNode.Trim, VarToStr(aValue).Trim]) ;
  Result := Self;
end;

function TXML2.Add(aNode: string; aSubNode: TXML2): TXML2;
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

function TXML2.Add(aNode: string; aAttributes: TVarArray; aValue: Variant): TXML2;
var
  Tmp: String;
begin
  Tmp := aAttributes.Ayracli(' ').Trim;
  _Source := _Source
           + _f('<%0:s%1:s%2:s>%3:s</%0:s%1:s>', [_NS, aNode.Trim, _if(Tmp.IsEmpty = True, '', ' ' + Tmp), VarToStr(aValue).Trim]) ;
  Result := Self;
end;

function TXML2.Add(aNode: string; aAttributes: TVarArray; aSubNode: TXML2): TXML2;
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

function TXML2.Add(aNode: string; aAttributes: TVarArray): TXML2;
var
  Tmp: String;
begin
  Tmp := aAttributes.Ayracli(' ').Trim;
  _Source := _Source + _f('<%0:s%1:s%2:s/>', [_NS, aNode.Trim, _if(Tmp.IsEmpty = True, '', ' ' + Tmp)]) ;
  Result := Self;
end;

function TXML2._f(const aFormat: string; const Args: array of const): string;
begin
  Result := Format(aFormat, Args);
end;

function TXML2._if(aKosul: Boolean; aTrue, aFalse: String): String;
begin
  if (aKosul = TRUE) then Result := aTrue else Result := aFalse;
end;

function TXML2._NS: String;
begin
  Result := _if( (_NameSpace.Trim.IsEmpty = True), '', _NameSpace.Trim+':');
end;

function TXML2.SaveToFile(aFileName: TFileName): TXML2;
var
  Dosya : TStreamWriter;
begin
  if (directoryExists(ExtractFileDir(aFileName), True) = TRUE) then begin
      try
        Dosya := TStreamWriter.Create(aFileName, False, TEncoding.UTF8);
        Dosya.Write(Self.AsString);
        Dosya.Close;
      finally
        FreeAndNil(Dosya);
      end;
  end else begin
      ShowMessage('Dosya adresinde belirtilen klasör yok');
  end;
  Result := Self;
end;

{ TVarArrayHelper }

function TVarArrayHelper.Ayracli(aAyrac: String): String;
var
 I: Integer;
begin
 for I := Low(Self) to High(Self)
  do if  (I < High(Self) )
     then Result := Result + VarToStrDef(Self[I], '').Trim + aAyrac
     else Result := Result + VarToStrDef(Self[I], '').Trim;
end;

{ TEncodingHelper }

function TEncodingHelper.AsEncoderName: String;
begin
  {---Kaynaklar-----------------------------------------------------------------------}
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
