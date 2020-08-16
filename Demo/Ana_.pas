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
            .FormatXml
            ;
      MM.Lines.Text := XML.AsString;
      XML.DisposeOf;
      //FreeAndNil(XML);
  end else
  {$endregion}
  {$region 'Kitap XML Örneği'}
  if (Sender = bt_EFatura) then begin
      XML := New('cac').Version(1.0).Encoding(TEncoding.UTF8)
            .StyleSheet('XSLT','Egemen.Xslt')
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
            .FormatXml
            ;
      MM.Text := XML.AsString;
      XML.DisposeOf;
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
