Welcome to the FluentXML

This unit demonstrates how we can produce an XML document in Object Pascal (Delphi) with a simple way of using the fluent design pattern and is offered to community service for this purpose.

Uğur PARLAYAN

http://www.potansif.com

###### for example;

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
   Memo1.Text := XML.SaveToFile(‘C:\Temp\Demo.xml’).AsString;
 finally
   FreeAndNil(XML);
 end;
end;
```

###### And then the output produced is as follows;


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

### New Features & Improvements

#### 1. Automatic XML Entity Escaping
Special characters (`&`, `<`, `>`, `"`, `'`) in node text values are automatically escaped to valid XML entities (`&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`) without affecting nested subnodes.

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

**Produced Output:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<Urunler>
	<Urun ID="1">
		<Adi>C++ &amp; Delphi</Adi>
		<Aciklama>Fiyat &lt; 100 TL &amp; Indirim %10</Aciklama>
	</Urun>
</Urunler>
```

#### 2. Thread-Safe Invariant Formatting
Number and float formatting uses an isolated static `TFormatSettings.Invariant` instance initialized once at startup. This eliminates global `FormatSettings` mutations and guarantees zero-overhead thread safety in multi-threaded applications.

---

### Advanced Enterprise Example (UBL E-Invoice Simulation)

This showcase demonstrates constructing a complex, multi-level E-Invoice (UBL 2.1) XML structure in a **single fluent statement**, combining attributes, namespaces, stylesheets, subnode arrays, automatic entity escaping, CDATA sections, and self-closing tags:

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

              // Self-closing node with multiple attributes
              New.Add('AdditionalDocumentReference', ['ID="REF-2026"', 'Scheme="E-Invoice"', 'Status="Active"']),

              // Supplier Party Details with Nested Hierarchy & Escaped Commercial Text
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

              // Customer Party Details
              New.Add('AccountingCustomerParty',
                  New.Add('Party',
                      New.Add('PartyIdentification', New.Add('ID', ['schemeID="TCKN"'], '98765432101'))
                         .Add('PartyName', New.Add('Name', 'Ahmet & Mehmet Ticaret Ltd. Şti.'))
                  )
              ),

              // Invoice Line Items Array with Dynamic Price Escaping and CDATA Notes
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

              // Legal Monetary Total Summary
              New.Add('LegalMonetaryTotal',
                  New.Add('LineExtensionAmount', ['currencyID="TRY"'], 2500.00)
                     .Add('TaxExclusiveAmount', ['currencyID="TRY"'], 2500.00)
                     .Add('TaxInclusiveAmount', ['currencyID="TRY"'], 2950.00)
                     .Add('AllowanceTotalAmount', ['currencyID="TRY"'], 0.00)
                     .Add('PayableAmount', ['currencyID="TRY"'], 2950.00)
              )
          ])
          .FormatXml;

    // Save formatted XML output to disk or stream
    XML.SaveToFile('C:\Temp\Enterprise_EInvoice.xml');
    Writeln(XML.AsString);
  finally
    FreeAndNil(XML);
  end;
end;
```

**Resulting Formatted XML Output:**

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


