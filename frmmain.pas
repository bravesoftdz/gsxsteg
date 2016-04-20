unit frmmain;

{$mode objfpc}{$H+}
{$i gsx.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, EditBtn, StdCtrls, LCLType, LCLIntf;

type
  { TMainForm }
  TMainForm = class(TForm)
    btnHide: TButton;
    btnExtract: TButton;
    btnSaveMessage: TButton;
    dlgTarget: TSaveDialog;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    lblCapacity: TLabel;
    lblLink: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblVersion: TLabel;
    PageControl1: TPageControl;
    dlgSaveMessage: TSaveDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    txtFileToHide: TFileNameEdit;
    txtMedium: TFileNameEdit;
    txtMedium1: TFileNameEdit;
    txtMessageToHide: TMemo;
    txtMessage: TMemo;
    txtPassword: TEdit;
    txtPassword1: TEdit;
    procedure btnExtractClick(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure btnSaveMessageClick(Sender: TObject);
    procedure lblLinkClick(Sender: TObject);
    procedure txtMediumAcceptFileName(Sender: TObject; var Value: String);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  StegImage;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnHideClick(Sender: TObject);
var
  si: TStegImage;
begin
  if not FileExists(txtMedium.FileName) then begin
    Application.MessageBox('Medium file does not exist', 'Error', MB_OK or MB_ICONERROR);
    txtMedium.SetFocus;
    Exit;
  end;
  if txtPassword.Text = '' then begin
    Application.MessageBox('Password can not be empty', 'Error', MB_OK or MB_ICONERROR);
    txtPassword.SetFocus;
    Exit;
  end;
  if not FileExists(txtFileToHide.FileName) and (txtMessageToHide.Text = '') then begin
    Application.MessageBox('Nothing to hide', 'Error', MB_OK or MB_ICONERROR);
    txtFileToHide.SetFocus;
    Exit;
  end;
  if dlgTarget.Execute then begin
    si := TStegImage.Create;
    try
      si.LoadFromFile(txtMedium.FileName);
      if FileExists(txtFileToHide.FileName) then begin
        si.LoadDataFromFile(txtFileToHide.FileName);
      end else begin
        si.LoadDataFromString(txtMessageToHide.Text);
      end;
      si.Password := txtPassword.Text;
      try
        si.Embed;
        si.SaveToFile(dlgTarget.FileName);
      except
        on E: Exception do
          Application.MessageBox(PChar(E.Message), 'Error', MB_OK or MB_ICONERROR);
      end;
    finally
      si.Free;
    end;
  end;
end;

procedure TMainForm.btnSaveMessageClick(Sender: TObject);
var
  si: TStegImage;
begin
  if not FileExists(txtMedium1.FileName) then begin
    Application.MessageBox('Medium file does not exist', 'Error', MB_OK or MB_ICONERROR);
    txtMedium1.SetFocus;
    Exit;
  end;
  if txtPassword1.Text = '' then begin
    Application.MessageBox('Password can not be empty', 'Error', MB_OK or MB_ICONERROR);
    txtPassword1.SetFocus;
    Exit;
  end;
  if dlgSaveMessage.Execute then begin
    si := TStegImage.Create;
    try
      si.LoadFromFile(txtMedium1.FileName);
      si.Password := txtPassword1.Text;
      try
        si.Extract;
        si.SaveDataToFile(dlgSaveMessage.FileName);
      except
        on E: Exception do
          Application.MessageBox(PChar(E.Message), 'Error', MB_OK or MB_ICONERROR);
      end;
    finally
      si.Free;
    end;
  end;
end;

procedure TMainForm.btnExtractClick(Sender: TObject);
var
  si: TStegImage;
begin
  if not FileExists(txtMedium1.FileName) then begin
    Application.MessageBox('Medium file does not exist', 'Error', MB_OK or MB_ICONERROR);
    txtMedium1.SetFocus;
    Exit;
  end;
  if txtPassword1.Text = '' then begin
    Application.MessageBox('Password can not be empty', 'Error', MB_OK or MB_ICONERROR);
    txtPassword1.SetFocus;
    Exit;
  end;
  si := TStegImage.Create;
  try
    si.LoadFromFile(txtMedium1.FileName);
    si.Password := txtPassword1.Text;
    try
      si.Extract;
      txtMessage.Text := si.GetDataAsString;
    except
      on E: Exception do
        Application.MessageBox(PChar(E.Message), 'Error', MB_OK or MB_ICONERROR);
    end;
  finally
    si.Free;
  end;
end;

procedure TMainForm.lblLinkClick(Sender: TObject);
begin
  OpenURL((Sender as TLabel).Caption);
end;

procedure TMainForm.txtMediumAcceptFileName(Sender: TObject; var Value: String);
var
  si: TStegImage;
begin
  si := TStegImage.Create;
  try
    try
      si.LoadFromFile(Value);
      lblCapacity.Caption := Format('Capacity: %d Byte', [si.Capacity]);
    except
      lblCapacity.Caption := 'Capacity: n/a';
    end;
  finally
    si.Free;
  end;
end;

end.

