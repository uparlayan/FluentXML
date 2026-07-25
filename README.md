# FluentXML Generator

> Object Pascal (Delphi) ile Fluent Design Pattern kullanarak kolay ve hızlı bir şekilde XML belgesi ve etiketleri üretmenizi sağlayan kütüphane.  
> A library that demonstrates how to produce XML documents in Object Pascal (Delphi) using the fluent design pattern.

---

## 📋 İçindekiler / Table of Contents

- [Hakkında / About](#hakkında--about)
- [Temel Kullanım Örneği / Basic Usage Example](#temel-kullanım-örneği--basic-usage-example)
- [Yeni Özellikler ve İyileştirmeler / New Features & Improvements](#yeni-özellikler-ve-iyileştirmeler--new-features--improvements)
  - [1. Otomatik XML Entity Escaping / Automatic XML Entity Escaping](#1-otomatik-xml-entity-escaping--automatic-xml-entity-escaping)
  - [2. İzole ve Güvenli Sayı Biçimlendirme / Thread-Safe Invariant Formatting](#2-izole-ve-güvenli-sayı-biçimlendirme--thread-safe-invariant-formatting)
  - [3. Veritabanı (TDataSet) Entegrasyonu / Database (TDataSet) Integration](#3-veritabanı-tdataset-entegrasyonu--database-tdataset-integration)
  - [4. Delphi IDE Paleti Temsilcisi (TFluentXmlComponent) / Delphi IDE Tool Palette Proxy](#4-delphi-ide-paleti-temsilcisi-tfluentxmlcomponent--delphi-ide-tool-palette-proxy)
- [Gelişmiş Kurumsal Örnek (E-Fatura / UBL 2.1) / Advanced Enterprise Example](#gelişmiş-kurumsal-örnek-e-fatura--ubl-21--advanced-enterprise-example)

---

## Hakkında / About

**TR:** Bu ünite, Object Pascal (Delphi) ortamında Akıcı Tasarım Deseni (Fluent Design Pattern) kullanılarak basit, esnek ve okunabilir bir şekilde XML belgeleri üretilmesini sağlar ve topluluğun hizmetine sunulmuştur.

**EN:** This unit demonstrates how we can produce an XML document in Object Pascal (Delphi) with a simple way of using the fluent design pattern and is offered to community service for this purpose.

**Yazar / Author:** Uğur PARLAYAN  
**Web:** http://www.potansif.com  

---

## Temel Kullanım Örneği / Basic Usage Example

**TR:** Aşağıdaki örnek, iç içe etiketler, CDATA blokları ve öznitelikler (attributes) içeren temel bir Kitaplar XML yapısının nasıl oluşturulduğunu gösterir:

**EN:** The following example demonstrates creating a basic Books XML structure with nested tags, CDATA blocks, and attributes:

```delphi
procedure TForm1.Button1Click(Sender: TObject);
var
 XML: TFluentXML;
begin
 try
   XML := New
         .Version(1.0)
         .Encoding(TEncoding.UTF8)
         .NameSpace('')
         .Add('Kitaplar'
             ,New
             .Add('Kitap', [ 'ID="1000"', 'Indirimli="Hayir"' ]
                 ,New
                 .Add('Adi'   , 'Mastering Delphi')
                 .Add('Fiyat' , 50)
                 .Add('Stok'  , 40)
                 .Add('Yazarlar'
                     ,New
                     .Add('Yazar', 'Marco CANTU')
                     .Add('Yazar', '<![CDATA[TEST]]>')
                     )
                 )
             .Add('Kitap', [ 'ID="1001"', 'Indirimli="Evet"' ]
                 ,New
                 .Add('Adi'   ,'PHP, MySQL ve Apache')
                 .Add('Fiyat' , 65)
                 .Add('Stok'  , 30)
                 .Add('Yazarlar'
                     ,New
                     .Add('Yazar', 'Julie C. MELONI')
                     .Add('Yazar', '<![CDATA[TEST]]>')
                     )
                 )
             .Add('Kitap', [ 'ID="1002"', 'Indirimli="Evet"' ]
                 ,New
                 .Add('Adi'   ,'Delphi Cookbook')
                 .Add('Fiyat' , 35)
                 .Add('Stok'  , 300)
                 .Add('Yazarlar'
                     ,New
                     .Add('Yazar', 'Daniele TETİ')
                     )
                 )
             )
         ;
   Memo1.Text := XML.SaveToFile('C:\Temp\Demo.xml').AsString;
 finally
   FreeAndNil(XML);
 end;
end;
```

**Üretilen Çıktı / Produced Output:**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Kitaplar>
	<Kitap ID="1000" Indirimli="Hayir">
		<Adi>Mastering Delphi</Adi>
		<Fiyat>50</Fiyat>
		<Stok>40</Stok>
		<Yazarlar>
			<Yazar>Marco CANTU</Yazar>
			<Yazar><![CDATA[TEST]]></Yazar>
		</Yazarlar>
	</Kitap>
	<Kitap ID="1001" Indirimli="Evet">
		<Adi>PHP, MySQL ve Apache</Adi>
		<Fiyat>65</Fiyat>
		<Stok>30</Stok>
		<Yazarlar>
			<Yazar>Julie C. MELONI</Yazar>
		</Yazarlar>
	</Kitap>
	<Kitap ID="1002" Indirimli="Evet">
		<Adi>Delphi Cookbook</Adi>
		<Fiyat>35</Fiyat>
		<Stok>300</Stok>
		<Yazarlar>
			<Yazar>Daniele TETİ</Yazar>
		</Yazarlar>
	</Kitap>
</Kitaplar>
```

---

## Yeni Özellikler ve İyileştirmeler / New Features & Improvements

### 1. Otomatik XML Entity Escaping / Automatic XML Entity Escaping

**TR:** Metin düğümlerindeki özel karakterler (`&`, `<`, `>`, `"`, `'`), alt düğümlerin etiket yapısını bozmadan otomatik olarak geçerli XML entity karşılıklarına (`&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`) dönüştürülür.

**EN:** Special characters (`&`, `<`, `>`, `"`, `'`) in node text values are automatically escaped to valid XML entities (`&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`) without affecting nested subnodes.

```delphi
procedure DemoEscaping;
var
  XML: TFluentXML;
begin
  try
    XML := New
          .Version(1.0)
          .Encoding(TEncoding.UTF8)
          .Add('Urunler', 
              New.Add('Urun', ['ID="1"'], 
                  New.Add('Adi', 'C++ & Delphi')
                     .Add('Aciklama', 'Fiyat < 100 TL & Indirim %10')
              )
          )
          .FormatXml;

    ShowMessage(XML.AsString);
  finally
    FreeAndNil(XML);
  end;
end;
```

**Üretilen Çıktı / Produced Output:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<Urunler>
	<Urun ID="1">
		<Adi>C++ &amp; Delphi</Adi>
		<Aciklama>Fiyat &lt; 100 TL &amp; Indirim %10</Aciklama>
	</Urun>
</Urunler>
```

### 2. İzole ve Güvenli Sayı Biçimlendirme / Thread-Safe Invariant Formatting

**TR:** Sayı ve ondalık biçimlendirmeleri, uygulama başlangıcında 1 kez ilklendirilen izole statik `TFormatSettings.Invariant` örneğini kullanır. Bu sayede global `FormatSettings` yan etkileri önlenir ve çoklu izlekli (multi-threaded) senaryolarda güvenli çalışma sağlanır.

**EN:** Number and float formatting uses an isolated static `TFormatSettings.Invariant` instance initialized once at startup. This eliminates global `FormatSettings` mutations and guarantees zero-overhead thread safety in multi-threaded applications.

### 3. Veritabanı (TDataSet) Entegrasyonu / Database (TDataSet) Integration

**TR:** `Add(aNodeName, aDataSet)` veya `Add(aNodeName, aRowName, aDataSet)` metodları ile herhangi bir Delphi `TDataSet` (örneğin `TFDQuery`, `TClientDataSet` vb.) verisi tek satırda otomatik olarak XML düğüm dizisine dönüştürülür.

**EN:** Any Delphi `TDataSet` (e.g. `TFDQuery`, `TClientDataSet`, etc.) data can be automatically converted into XML node structures in a single line using `Add(aNodeName, aDataSet)` or `Add(aNodeName, aRowName, aDataSet)`.

```delphi
procedure DemoDataSetToXML(aQuery: TDataSet);
var
  XML: TFluentXML;
begin
  try
    // Veritabanı sorgu sonucunu doğrudan XML düğümlerine aktarır
    XML := New.Root('MusteriListesi')
              .Add('Musteriler', 'Musteri', aQuery)
              .FormatXml;

    ShowMessage(XML.AsString);
  finally
    FreeAndNil(XML);
  end;
end;
```

### 4. Delphi IDE Paleti Temsilcisi (TFluentXmlComponent) / Delphi IDE Tool Palette Proxy

**TR:** `TFluentXML` hafif ve esnek bir `TObject` yapısı olarak korunurken, Delphi IDE Tool Palette (Bileşen Paleti) ve Form / DataModule tasarımı tercih eden geliştiriciler için `TFluentXmlComponent` temsilci bileşeni tasarlanmıştır. `FluentPack.dpk` paketi kurulduğunda `FluentXML` sekmesinde yer alır.

**EN:** While `TFluentXML` remains a lightweight and flexible `TObject`, `TFluentXmlComponent` proxy component is designed for developers who prefer the Delphi IDE Tool Palette and Form / DataModule designer. It is registered under the `FluentXML` palette tab when `FluentPack.dpk` is installed.

```delphi
procedure TForm1.Button1Click(Sender: TObject);
begin
  // Component Palette üzerinden sürüklenen TFluentXmlComponent kullanımı
  FluentXmlComponent1.Xml
    .Version(1.0)
    .Encoding(TEncoding.UTF8)
    .Add('Mesaj', 'Component Palette üzerinden oluşturuldu!')
    .FormatXml;

  ShowMessage(FluentXmlComponent1.Xml.AsString);
end;
```

---

## Gelişmiş Kurumsal Örnek (E-Fatura / UBL 2.1) / Advanced Enterprise Example

**TR:** Bu gövde gösterisi örneği; öznitelikler (attributes), namespace'ler, stylesheet'ler, alt düğüm dizileri, otomatik entity escaping, CDATA bölümleri ve kendi kapanan etiketlerin **tek bir akıcı (fluent) ifadede** nasıl birleştirildiğini karmaşık bir E-Fatura (UBL 2.1) yapısı üzerinde sergilemektedir:

**EN:** This showcase demonstrates constructing a complex, multi-level E-Invoice (UBL 2.1) XML structure in a **single fluent statement**, combining attributes, namespaces, stylesheets, subnode arrays, automatic entity escaping, CDATA sections, and self-closing tags:

```delphi
procedure BuildEnterpriseEInvoice;
var
  XML: TFluentXML;
begin
  try
    XML := New
          .Version(1.0)
          .Encoding(TEncoding.UTF8)
          .StyleSheet('text/xsl', 'https://efatura.gov.tr/stylesheets/general.xsl')
          .Root('Invoice')
          .NameSpace('cbc')
          .Add([
              New.Add('UBLVersionID', '2.1')
                 .Add('CustomizationID', 'TR1.2')
                 .Add('ProfileID', 'TICARETFATURA')
                 .Add('ID', 'GIB2026000000001')
                 .Add('CopyIndicator', 'false')
                 .Add('UUID', 'a8e9d3c1-7b4f-4e2a-9f12-001122334455')
                 .Add('IssueDate', '2026-07-25')
                 .Add('IssueTime', '22:50:00')
                 .Add('InvoiceTypeCode', 'SATIS'),

              // Öznitelikli kendi kapanan düğüm / Self-closing node with multiple attributes
              New.Add('AdditionalDocumentReference', ['ID="REF-2026"', 'Scheme="E-Invoice"', 'Status="Active"']),

              // Tedarikçi Taraf Detayları / Supplier Party Details with Nested Hierarchy
              New.Add('AccountingSupplierParty',
                  New.Add('Party',
                      New.Add('PartyIdentification', New.Add('ID', ['schemeID="VKN"'], '1234567890'))
                         .Add('PartyName', New.Add('Name', 'Parlayan Bilişim & Yazılım A.Ş.'))
                         .Add('PostalAddress',
                             New.Add('StreetName', 'Teknoloji Cad. No: 42/A')
                                .Add('CitySubdivisionName', 'Kadıköy')
                                .Add('CityName', 'İstanbul')
                                .Add('Country', New.Add('Name', 'Türkiye'))
                         )
                         .Add('PartyTaxScheme', New.Add('TaxScheme', New.Add('Name', 'Karasu V.D.')))
                  )
              ),

              // Müşteri Taraf Detayları / Customer Party Details
              New.Add('AccountingCustomerParty',
                  New.Add('Party',
                      New.Add('PartyIdentification', New.Add('ID', ['schemeID="TCKN"'], '98765432101'))
                         .Add('PartyName', New.Add('Name', 'Ahmet & Mehmet Ticaret Ltd. Şti.'))
                  )
              ),

              // Kalem Satırları Dizisi / Invoice Line Items Array
              New.Add('InvoiceLine',
                  New.Add('ID', '1')
                     .Add('InvoicedQuantity', ['unitCode="C62"'], 100)
                     .Add('LineExtensionAmount', ['currencyID="TRY"'], 2500.00)
                     .Add('Item',
                         New.Add('Description', 'Yüksek Hızlı Delphi & Pascal Yazılım Paket <V2.0>')
                            .Add('Name', 'FluentXML Pro Suite')
                            .Add('SellersItemIdentification', New.Add('ID', 'SKU-8891'))
                     )
                     .Add('Price', New.Add('PriceAmount', ['currencyID="TRY"'], 25.00))
                     .Add('Note', '<![CDATA[Kampanya İndirimi: 500 TL (KDV Dahil & Sepette Özel)]]>')
              ),

              // Genel Toplam Özeti / Legal Monetary Total Summary
              New.Add('LegalMonetaryTotal',
                  New.Add('LineExtensionAmount', ['currencyID="TRY"'], 2500.00)
                     .Add('TaxExclusiveAmount', ['currencyID="TRY"'], 2500.00)
                     .Add('TaxInclusiveAmount', ['currencyID="TRY"'], 2950.00)
                     .Add('AllowanceTotalAmount', ['currencyID="TRY"'], 0.00)
                     .Add('PayableAmount', ['currencyID="TRY"'], 2950.00)
              )
          ])
          .FormatXml;

    // Dosyaya kaydet veya çıktı al / Save to file or export string
    XML.SaveToFile('C:\Temp\Enterprise_EInvoice.xml');
    Writeln(XML.AsString);
  finally
    FreeAndNil(XML);
  end;
end;
```

**Üretilen Biçimlendirilmiş XML Çıktısı / Resulting Formatted XML Output:**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<?xml-stylesheet type="text/xsl" href="https://efatura.gov.tr/stylesheets/general.xsl"?>
<cbc:Invoice>
	<cbc:UBLVersionID>2.1</cbc:UBLVersionID>
	<cbc:CustomizationID>TR1.2</cbc:CustomizationID>
	<cbc:ProfileID>TICARETFATURA</cbc:ProfileID>
	<cbc:ID>GIB2026000000001</cbc:ID>
	<cbc:CopyIndicator>false</cbc:CopyIndicator>
	<cbc:UUID>a8e9d3c1-7b4f-4e2a-9f12-001122334455</cbc:UUID>
	<cbc:IssueDate>2026-07-25</cbc:IssueDate>
	<cbc:IssueTime>22:50:00</cbc:IssueTime>
	<cbc:InvoiceTypeCode>SATIS</cbc:InvoiceTypeCode>
	<cbc:AdditionalDocumentReference ID="REF-2026" Scheme="E-Invoice" Status="Active"/>
	<cbc:AccountingSupplierParty>
		<cbc:Party>
			<cbc:PartyIdentification>
				<cbc:ID schemeID="VKN">1234567890</cbc:ID>
			</cbc:PartyIdentification>
			<cbc:PartyName>
				<cbc:Name>Parlayan Bilişim &amp; Yazılım A.Ş.</cbc:Name>
			</cbc:PartyName>
			<cbc:PostalAddress>
				<cbc:StreetName>Teknoloji Cad. No: 42/A</cbc:StreetName>
				<cbc:CitySubdivisionName>Kadıköy</cbc:CitySubdivisionName>
				<cbc:CityName>İstanbul</cbc:CityName>
				<cbc:Country>
					<cbc:Name>Türkiye</cbc:Name>
				</cbc:Country>
			</cbc:PostalAddress>
			<cbc:PartyTaxScheme>
				<cbc:TaxScheme>
					<cbc:Name>Karasu V.D.</cbc:Name>
				</cbc:TaxScheme>
			</cbc:PartyTaxScheme>
		</cbc:Party>
	</cbc:AccountingSupplierParty>
	<cbc:AccountingCustomerParty>
		<cbc:Party>
			<cbc:PartyIdentification>
				<cbc:ID schemeID="TCKN">98765432101</cbc:ID>
			</cbc:PartyIdentification>
			<cbc:PartyName>
				<cbc:Name>Ahmet &amp; Mehmet Ticaret Ltd. Şti.</cbc:Name>
			</cbc:PartyName>
		</cbc:Party>
	</cbc:AccountingCustomerParty>
	<cbc:InvoiceLine>
		<cbc:ID>1</cbc:ID>
		<cbc:InvoicedQuantity unitCode="C62">100</cbc:InvoicedQuantity>
		<cbc:LineExtensionAmount currencyID="TRY">2500.00</cbc:LineExtensionAmount>
		<cbc:Item>
			<cbc:Description>Yüksek Hızlı Delphi &amp; Pascal Yazılım Paket &lt;V2.0&gt;</cbc:Description>
			<cbc:Name>FluentXML Pro Suite</cbc:Name>
			<cbc:SellersItemIdentification>
				<cbc:ID>SKU-8891</cbc:ID>
			</cbc:SellersItemIdentification>
		</cbc:Item>
		<cbc:Price>
			<cbc:PriceAmount currencyID="TRY">25.00</cbc:PriceAmount>
		</cbc:Price>
		<cbc:Note><![CDATA[Kampanya İndirimi: 500 TL (KDV Dahil & Sepette Özel)]]></cbc:Note>
	</cbc:InvoiceLine>
	<cbc:LegalMonetaryTotal>
		<cbc:LineExtensionAmount currencyID="TRY">2500.00</cbc:LineExtensionAmount>
		<cbc:TaxExclusiveAmount currencyID="TRY">2500.00</cbc:TaxExclusiveAmount>
		<cbc:TaxInclusiveAmount currencyID="TRY">2950.00</cbc:TaxInclusiveAmount>
		<cbc:AllowanceTotalAmount currencyID="TRY">0.00</cbc:AllowanceTotalAmount>
		<cbc:PayableAmount currencyID="TRY">2950.00</cbc:PayableAmount>
	</cbc:LegalMonetaryTotal>
</cbc:Invoice>
```
