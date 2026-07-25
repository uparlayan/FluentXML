program EFaturaDemo;

uses
  Vcl.Forms,
  Ana_ in 'Ana_.pas' {Ana},
  FluentXML_ in '..\Src\FluentXML_.pas',
  InvoiceType_Class_ in 'Classes\InvoiceType_Class_.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAna, Ana);
  Application.Run;
end.
