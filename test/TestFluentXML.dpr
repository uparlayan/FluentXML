program TestFluentXML;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  FluentXML_ in '..\Src\FluentXML_.pas';

procedure TestSubNodeEscaping;
var
  ParentNode, SubNode: TFluentXML;
begin
  Writeln('=== TEST 1: ALT DUGUM VE METIN ESCAPING TESTI ===');
  SubNode := New.Add('Adi', 'C++ & Delphi');
  ParentNode := New.Add('Kitap', ['ID="100"'], SubNode);
  Writeln('Sonuc XML:');
  Writeln(ParentNode.AsString);
  Writeln;
  FreeAndNil(ParentNode);
end;

procedure TestVariantEscaping;
var
  Xml: TFluentXML;
begin
  Writeln('=== TEST 2: DUZ METIN (VARIANT) ESCAPING TESTI ===');
  Xml := New.Add('Aciklama', 'Fiyat < 100 TL & Indirim %10');
  Writeln('Sonuc XML:');
  Writeln(Xml.AsString);
  Writeln('====================================================');
  FreeAndNil(Xml);
end;

begin
  try
    TestSubNodeEscaping;
    TestVariantEscaping;
  except
    on E: Exception do
      Writeln('HATA: ', E.Message);
  end;
  readln;
end.
