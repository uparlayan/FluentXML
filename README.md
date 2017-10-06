Welcome to the FluentXML

This unit demonstrates how we can produce an XML document in Object Pascal (Delphi) with a simple way of using the fluent design pattern and is offered to community service for this purpose.

Uğur PARLAYAN

http://www.rubicube.com.tr

###### for example;

```delphi
procedure TForm1.Button2Click(Sender: TObject);
var
 XML: TFluentXML;
begin
 try
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
   Memo1.Text := XML.SaveToFile(‘C:\Temp\Demo.xml’).AsString;
 finally
   FreeAndNil(XML);
 end;
end;
```
