{-------------------------------------------------------------------------------
-  Author      : Uğur PARLAYAN                                                 -
-  Email       : ugurparlayan@gmail.com                                        -
-  Class Name  : TFluentXML Generator.                                         -
-  Description : This unit demonstrates how we can produce an XML document     -
-                in Object Pascal (Delphi) with a simple way of using          -
-                the Fluent Design pattern and is offered to                   -
-                community service for this purpose.                           -
-  Create Date : 2017-09-12                                                    -
-  License     : GPL-3.0                                                       -
-------------------------------------------------------------------------------}
unit XML_;

interface

uses
  System.SysUtils, System.StrUtils, System.Variants, System.Classes, Vcl.Dialogs;

type
  TFluentXML = class
    type
      TVarArray = array of Variant;
      TVarArrayHelper = record helper for TVarArray
        function Concat(aAyrac: String): String;
      end;
      TEncodingHelper = class Helper for TEncoding
        function AsEncoderName: String;
      end;
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
      function AsString: String; // Bu noktada zincir kırılır...
      function Version(Value: Double): TFluentXML;
      function Encoding(Value: TEncoding): TFluentXML;
      function NameSpace(Value: String): TFluentXML;
      function Add(aNode: string): TFluentXML; overload;
      function Add(aNode: string; aValue: Variant): TFluentXML; overload;
      function Add(aNode: string; aSubNode: TFluentXML): TFluentXML; overload;
      function Add(aNode: string; aAttributes: TVarArray): TFluentXML; overload;
      function Add(aNode: string; aAttributes: TVarArray; aValue: Variant): TFluentXML; overload;
      function Add(aNode: string; aAttributes: TVarArray; aSubNode: TFluentXML): TFluentXML; overload;
      function SaveToFile(aFileName: TFileName): TFluentXML;
      class function New(aVersion: Double; aEncoding: TEncoding): TFluentXML;
  end;
  function FluentXML: TFluentXML;

implementation

function FluentXML: TFluentXML;
begin
  Result := TFluentXML.Create;
end;

{ TXML2 }

function TFluentXML.AsString: String;
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
  Result := StringReplace(_Source, '><', '>'#13#10'<', [rfReplaceAll, rfIgnoreCase]); // CDATA içinde geçerse sıkıntı olabilir...
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

function TFluentXML.Encoding(Value: TEncoding): TFluentXML;
begin
  _Encoding := Value;
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

function TFluentXML.Add(aNode: string; aAttributes: TVarArray; aValue: Variant): TFluentXML;
var
  Tmp: String;
begin
  Tmp := aAttributes.Concat(' ').Trim;
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
  Tmp := aAttributes.Concat(' ');
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

function TFluentXML.TVarArrayHelper.Concat(aAyrac: String): String;
var
 I: Integer;
begin
 for I := Low(Self) to High(Self)
  do if  (I < High(Self) )
     then Result := Result + VarToStrDef(Self[I], '').Trim + aAyrac
     else Result := Result + VarToStrDef(Self[I], '').Trim;
end;

{ TEncodingHelper }

function TFluentXML.TEncodingHelper.AsEncoderName: String;
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
