program SMSSender;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {Form1},
  DataModule in 'DataModule.pas' {DM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDM, DM);
  Application.Run;
end.

