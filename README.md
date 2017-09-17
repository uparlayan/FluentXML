Welcome to the FluentXML

This unit demonstrates how we can produce an XML document in Object Pascal (Delphi) with a simple way of using the fluent design pattern and is offered to community service for this purpose.

`procedure TForm1.Button2Click(Sender: TObject);
var
  XML: TXML2;
begin
  try
    XML := TXML2.Yeni(1.0, nil)
          .Version(1.0)
          .Encoding(TEncoding.UTF8)
          .NameSpace('')
          .Add('Kutuphane'
              ,NewXML
              .Add('Kitap', [ 'ID="1000"', 'Indirimli="Hayir"' ]
                  ,NewXML
                  .Add('Adi'   , 'Mastering Delphi')
                  .Add('Fiyat' , 50)
                  .Add('Stok'  , 40)
                  .Add('Yazarlar'
                      ,NewXML
                      .Add('Yazar', 'Marco CANTU')
                      )
                  )
              .Add('Kitap', [ 'ID="1001"', 'Indirimli="Evet"' ]
                  ,NewXML
                  .Add('Adi'   ,'PHP, MySQL ve Apache')
                  .Add('Fiyat' , 65)
                  .Add('Stok'  , 30)
                  .Add('Yazarlar'
                      ,NewXML
                      .Add('Yazar', 'Julie C. MELONI')
                      )
                  )
              .Add('Kitap', [ 'ID="1002"', 'Indirimli="Evet"' ]
                  ,NewXML
                  .Add('Adi'   ,'Delphi Cookbook')
                  .Add('Fiyat' , 35)
                  .Add('Stok'  , 300)
                  .Add('Yazarlar'
                      ,NewXML
                      .Add('Yazar', 'Daniele TETİ')
                      )
                  )
              )
          ;
    Memo1.Text := XML.SaveToFile('C:\Temp\Demo.xml').AsString;
  finally
    FreeAndNil(XML);
  end;
end;`
