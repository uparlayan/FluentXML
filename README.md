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
