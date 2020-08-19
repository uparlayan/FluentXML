program EFaturaDemo;

uses
  Vcl.Forms,
  Ana_ in 'Ana_.pas' {Ana},
  EFatura_Class_ in 'Classes\EFatura_Class_.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAna, Ana);
  Application.Run;
end.
