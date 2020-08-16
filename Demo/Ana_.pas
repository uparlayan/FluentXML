unit Ana_;

interface

uses
  FluentXML_,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TAna = class(TForm)
    bt_Olustur: TButton;
    MM: TMemo;
    Panel: TPanel;
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
  if (Sender = bt_Olustur) then begin
      XML := New
            .Version(1.0)
            .Encoding(TEncoding.UTF8)
            .NameSpace('')
            .Add('Kutuphane'
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
      MM.Text := XML.FormatXml.AsString;
      FreeAndNil(XML);
  end;
end;

end.
