unit Ana_;

interface

uses
  FluentXML_,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TAna = class(TForm)
    bt_Kitap: TButton;
    MM: TMemo;
    Panel1: TPanel;
    bt_EFatura: TButton;
    procedure KlikManager(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Ana: TAna;

implementation

{$R *.dfm}

procedure TAna.KlikManager(Sender: TObject);
var
  XML : TFluentXML;
begin
  {$region 'Kitap XML Örneği'}
  if (Sender = bt_Kitap) then begin
      XML := New
            .Version(1.0)
            .Encoding(TEncoding.UTF8)
            .root('Kitaplar')
            .Add([New
                 .Add('Kitap', [ 'ID="1000"', 'Indirimli="Hayir"' ]
                     ,New
                     .Add('Adi'   , 'Tüm Platformlar için Hızlı Uygulama Geliştirme')
                     .Add('Fiyat' , 250)
                     .Add('Stok'  , 400)
                     .Add('Yazarlar'
                         ,New
                         .Add('Yazar', 'Fesih ARSLAN')
                         )
                     .Add('Tasarim'
                         ,New
                         .Add('KapakTasarimi', 'Nihat DOMUR')
                         .Add('SayfaDuzeni', 'Uğur PARLAYAN')
                         )
                     .Add('Kunye'
                         ,New
                         .Add('YayinYonetmeni', 'Yücel TEPEKÖY')
                         .Add('YayinKoordinatoru', 'Ahmet Sadi TEPEKÖY')
                         .Add('MatbaaSertifika No', '19371')
                         .Add('Yayincilik Sertifika No', '42989')
                         .Add('ISBN', '978-605-80936-0-7')
                         )
                     )
                 .Add('Kitap', [ 'ID="1001"', 'Indirimli="Hayir"' ]
                     ,New
                     .Add('Adi'   , 'Mastering Delphi')
                     .Add('Fiyat' , 50)
                     .Add('Stok'  , 40)
                     .Add('Yazarlar'
                         ,New
                         .Add('Yazar', 'Marco CANTU')
                         )
                     )
                 .Add('Kitap', [ 'ID="1002"', 'Indirimli="Evet"' ]
                     ,New
                     .Add('Adi'   ,'PHP, MySQL ve Apache')
                     .Add('Fiyat' , 65)
                     .Add('Stok'  , 30)
                     .Add('Yazarlar'
                         ,New
                         .Add('Yazar', 'Julie C. MELONI')
                         )
                     )
                 .Add('Kitap', [ 'ID="1003"', 'Indirimli="Evet"' ]
                     ,New
                     .Add('Adi'   ,'Delphi Cookbook')
                     .Add('Fiyat' , 35)
                     .Add('Stok'  , 300)
                     .Add('Yazarlar'
                         ,New
                         .Add('Yazar', 'Daniele TETİ')
                         )
                     )
                ])
            .FormatXml
            ;
      MM.Lines.Text := XML.AsString;
      XML.DisposeOf; // OR ! FreeAndNil(XML);
  end else
  {$endregion}
  {$region 'Kitap XML Örneği'}
  if (Sender = bt_EFatura) then begin
      XML := New.Version(1.0).Encoding(TEncoding.UTF8)
            .StyleSheet('XSLT','EFatura.xslt')
            .Add('Invoices'
                ,New.Add('Invoice'
                        ,[ 'xmlns="urn:oasis:names:specification:ubl:schema:xsd:Invoice-2"', 'xmlns:cac="urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2"'
                         , 'xmlns:cbc="urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2"', 'xmlns:ccts="urn:un:unece:uncefact:documentation:2"'
                         , 'xmlns:ds="http://www.w3.org/2000/09/xmldsig#"', 'xmlns:ext="urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2"'
                         , 'xmlns:qdt="urn:oasis:names:specification:ubl:schema:xsd:QualifiedDatatypes-2"', 'xmlns:ubltr="urn:oasis:names:specification:ubl:schema:xsd:TurkishCustomizationExtensionComponents"'
                         , 'xmlns:udt="urn:un:unece:uncefact:data:specification:UnqualifiedDataTypesSchemaModule:2"', 'xmlns:xades="http://uri.etsi.org/01903/v1.3.2#"'
                         , 'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"', 'xsi:schemaLocation="urn:oasis:names:specification:ubl:schema:xsd:Invoice-2 UBL-Invoice-2.1.xsd"'
                         ]
                        ,New('ext').Add('UBSExtensions', New.Add('UBLExtension', ''))
                        .NameSpace('cbc')
                        .Add('AccountingSupplierParty'
                            ,New('cac').Add('Party'
                                       ,New
                                       .Add([New('cbc').Add('WebSiteURI', 'http://www.aaa.com.tr')
                                            ,New('cac').Add('PartyIdentification'
                                                           ,New('cbc').Add('ID', ['schemaID="VKN"']  , '1234567890') )
                                            ,New('cac').Add('PartyName'
                                                           ,New('cbc').Add('Name', 'AAA Anonim Şirketi') )
                                            ,New('cac').Add('PostalAddress'
                                                           ,New([New('cbc').Add('ID'                 , '1234567890')
                                                                ,New('cbc').Add('StreetName'         , 'Papatya Cad. Yasemin Sokak')
                                                                ,New('cbc').Add('BuildingNumber'     , '21')
                                                                ,New('cbc').Add('CitySubDivisionName', 'Beşiktaş')
                                                                ,New('cbc').Add('CityName'           , 'İstanbul')
                                                                ,New('cbc').Add('PostalZone'         , '34100')
                                                                ,New('cac').Add('Country'
                                                                               ,New('cbc').Add('Name', 'Türkiye')
                                                                               )
                                                               ])
                                                           )
                                            ,New('cac').Add('PartyTaxScheme'
                                                           ,New('cbc').Add('TaxSchema'
                                                                          ,New('cbc').Add('Name', 'Büyük Mükellefler')
                                                                          )
                                                           )
                                            ,New('cac').Add('Contact'
                                                           ,New([New('cbc').Add('Telephone'      , '(537) 953 0593')
                                                                ,New('cbc').Add('Telefax'        , '(537) 953 0593')
                                                                ,New('cbc').Add('ElectronicMail' , 'ugurparlayan@gmail.com')
                                                               ])
                                                           )
                                            ])
                                       )
                            )
                      )
                  )
            .FormatXml
            ;
      MM.Text := XML.AsString;
      XML.DisposeOf; // OR ! FreeAndNil(XML);
  end else
  {$endregion}
  {$region 'Boş if şablonu'}
  if (Sender = nil) then begin
  end else
  {$endregion}
  {$region 'else'}
  begin
    { Standart else sonu }
  end;
  {$endregion}
end;

end.
