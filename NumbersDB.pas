unit NumbersDB;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ListView,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, Data.Bind.EngExt, FMX.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.Components,
  Data.Bind.DBScope, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FMX.Objects;

type
  TNumbersDBForm = class(TForm)
    ListViewNumbersSidebar: TListView;
    Panel2: TPanel;
    EditNumber: TEdit;
    SpeedButtonSaveNumber: TSpeedButton;
    FDTableNumbers: TFDTable;
    FDTableNumbersid: TFDAutoIncField;
    FDTableNumberstype_id: TIntegerField;
    FDTableNumbersnumber: TIntegerField;
    FDTableNumberssent_cnt: TIntegerField;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    BindSourceDB1: TBindSourceDB;
    Rectangle1: TRectangle;
    procedure SpeedButtonSaveNumberClick(Sender: TObject);
    procedure ListViewNumbersSidebarDeletingItem(Sender: TObject;
      AIndex: Integer; var ACanDelete: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NumbersDBForm: TNumbersDBForm;

implementation

{$R *.fmx}

uses main, DataModule;

procedure TNumbersDBForm.FormCreate(Sender: TObject);
begin
  self.FDTableNumbers.Active := True;
end;

procedure TNumbersDBForm.ListViewNumbersSidebarDeletingItem(Sender: TObject;
  AIndex: Integer; var ACanDelete: Boolean);
begin
  FDTableNumbers.Delete;
  FDTableNumbers.Refresh;
end;

procedure TNumbersDBForm.SpeedButtonSaveNumberClick(Sender: TObject);
begin
  if not EditNumber.Text.IsEmpty then
  begin
    SpeedButtonSaveNumber.ImageIndex := 1;
    with FDTableNumbers do
    begin
      Insert;
      fieldByName('number').AsInteger := EditNumber.Text.ToInteger;
      fieldByName('sent_cnt').AsInteger := 0;
      Post;
    end;
    FDTableNumbers.Refresh;
    SpeedButtonSaveNumber.ImageIndex := 0;
  end
  else
  begin
    ShowMessage('Fill phone number!');
  end;
end;

end.
