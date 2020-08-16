program FluentXml_Demo;

uses
  Vcl.Forms,
  Ana_ in 'Ana_.pas' {Ana},
  FluentXML_ in '..\Src\FluentXML_.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAna, Ana);
  Application.Run;
end.
