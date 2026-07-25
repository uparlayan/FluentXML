program TestFluentXML;

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  Data.DB,
  Datasnap.DBClient,
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
  Writeln;
  FreeAndNil(Xml);
end;

procedure TestSaveToFileRelative;
var
  Xml: TFluentXML;
  TestFile: String;
begin
  Writeln('=== TEST 3: BAGIL YOL ILE SaveToFile TESTI ===');
  TestFile := 'TestRelativeSave.xml';
  Xml := New
        .Version(1.0)
        .Encoding(TEncoding.UTF8)
        .Root('TestKayit')
        .Add('Mesaj', 'Bağıl Yol İle Kayıt Başarılı!')
        .Add('Zaman', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now))
        .FormatXml;

  Xml.SaveToFile(TestFile);
  if FileExists(TestFile) then
    Writeln('BASARILI: Duzgun XML dosyasi kaydedildi: ', ExpandFileName(TestFile))
  else
    Writeln('HATA: Dosya olusturulamadi!');
  Writeln;
  FreeAndNil(Xml);
end;

procedure TestFormatXmlPerformance;
var
  Xml: TFluentXML;
  I: Integer;
  StartTicks, EndTicks: DWORD;
  FileName: String;
begin
  Writeln('=== TEST 4: OPTIMIZE EDILMIS FormatXml PERFORMANS VE DOSYA KAYIT TESTI ===');
  FileName := 'TestPerformance5000.xml';
  Xml := New
        .Version(1.0)
        .Encoding(TEncoding.UTF8)
        .Root('TopluVeri');

  for I := 1 to 5000 do
  begin
    Xml.Add('Urun', ['ID="' + IntToStr(I) + '"'], New.Add('Adi', 'Urun & Model ' + IntToStr(I)).Add('Fiyat', I * 1.5));
  end;

  StartTicks := GetTickCount;
  Xml.FormatXml;
  EndTicks := GetTickCount;

  Xml.SaveToFile(FileName);

  Writeln('5000 Dugumlu XML Bicipmlendirme Suresi: ', EndTicks - StartTicks, ' ms');
  Writeln('Uretilen XML Karakter Uzunlugu: ', Length(Xml.AsString));
  Writeln('5000 Elemanli XML Dosyasi Diske Kaydedildi: ', ExpandFileName(FileName));
  Writeln;
  FreeAndNil(Xml);
end;

procedure TestDataSetConversion;
var
  Xml: TFluentXML;
  MemTable: TClientDataSet;
begin
  Writeln('=== TEST 5: TDataSet (DB) CONVERSION TESTI ===');
  MemTable := TClientDataSet.Create(nil);
  try
    MemTable.FieldDefs.Add('ID', ftInteger);
    MemTable.FieldDefs.Add('Musteri', ftString, 50);
    MemTable.FieldDefs.Add('Bakiye', ftFloat);
    MemTable.CreateDataSet;

    MemTable.AppendRecord([1, 'Ahmet & Mehmet A.Ş.', 1500.50]);
    MemTable.AppendRecord([2, 'Karasu Yazılım <Tech>', 3400.00]);

    Xml := New.Root('MusteriListesi').Add('Musteriler', 'Musteri', MemTable).FormatXml;
    Writeln('Sonuc XML:');
    Writeln(Xml.AsString);
    Writeln('====================================================');
    FreeAndNil(Xml);
  finally
    MemTable.Free;
  end;
end;

begin
  try
    TestSubNodeEscaping;
    TestVariantEscaping;
    TestSaveToFileRelative;
    TestFormatXmlPerformance;
    TestDataSetConversion;
  except
    on E: Exception do
      Writeln('HATA: ', E.Message);
  end;
end.
