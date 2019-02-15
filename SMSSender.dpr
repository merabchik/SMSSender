program SMSSender;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {mainForm},
  DataModule in 'DataModule.pas' {DM: TDataModule},
  NumbersDB in 'NumbersDB.pas' {NumbersDBForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TmainForm, mainForm);
  Application.CreateForm(TDM, DM);
  Application.Run;
end.

