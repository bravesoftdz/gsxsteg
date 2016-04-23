{
  gsxsteg
  2016, sa
}

unit frmmain;

{$mode objfpc}{$H+}
{$i gsx.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, EditBtn, StdCtrls, LCLType, LCLIntf, Histogram, StegHisto;

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
    lblCompileDate: TLabel;
    lblCompiler: TLabel;
    lblUsedToHide: TLabel;
    lblCapacity: TLabel;
    lblFilesize: TLabel;
    lblMsgsize: TLabel;
    lblLink: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblUsedToExtract: TLabel;
    lblVersion: TLabel;
    PageControl1: TPageControl;
    dlgSaveMessage: TSaveDialog;
    pnlHisto: TPanel;
    pnlHistoFile: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    txtFileToHide: TFileNameEdit;
    txtMedium: TFileNameEdit;
    txtMedium1: TFileNameEdit;
    txtHistoFile: TFileNameEdit;
    txtMessageToHide: TMemo;
    txtMessage: TMemo;
    txtPassword: TEdit;
    txtPassword1: TEdit;
    procedure btnExtractClick(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure btnSaveMessageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblLinkClick(Sender: TObject);
    procedure txtFileToHideAcceptFileName(Sender: TObject; var Value: String);
    procedure txtHistoFileAcceptFileName(Sender: TObject; var Value: String);
    procedure txtMediumAcceptFileName(Sender: TObject; var Value: String);
    procedure txtMessageToHideChange(Sender: TObject);
  private
    { private declarations }
    FStegHisto: TStegHisto;
    FHistogram: THistogram;
    procedure AppException(Sender: TObject; E: Exception);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  StegImage;

{$R *.lfm}

{$I version.inc}

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
        lblUsedToHide.Caption := Format('Used %d of %d Pixel', [si.UsedPixel, si.MaxPixel]);
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
        lblUsedToExtract.Caption := Format('Used %d of %d Pixel', [si.UsedPixel, si.MaxPixel]);
      except
        on E: Exception do
          Application.MessageBox(PChar(E.Message), 'Error', MB_OK or MB_ICONERROR);
      end;
    finally
      si.Free;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  lblVersion.Caption := Format('Version %s', [VERSION]);
  lblCompiler.Caption := 'Compiled with FPC ' + {$i %FPCVERSION%} + ' for ' + {$I %FPCTARGETOS%} + ' on ' + {$I %FPCTARGETCPU%};
  lblCompileDate.Caption := {$i %DATE%} + ' ' + {$i %TIME%};
  Application.OnException := @AppException;

  FHistogram := THistogram.Create(pnlHisto);
  FHistogram.Parent := pnlHisto;
  FHistogram.Align := alClient;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FHistogram.StegHisto := nil;
  if Assigned(FStegHisto) then
    FStegHisto.Free;
end;

procedure TMainForm.AppException(Sender: TObject; E: Exception);
begin
  // Ignore focus stuff when switching tabs
  if not (E is EInvalidOperation) then
    Application.HandleException(Sender);
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
      lblUsedToExtract.Caption := Format('Used %d of %d Pixel', [si.UsedPixel, si.MaxPixel]);
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

procedure TMainForm.txtFileToHideAcceptFileName(Sender: TObject;
  var Value: String);
begin
  if FileExists(Value) then
    lblFilesize.Caption := Format('Size: %d Byte', [FileUtil.FileSize(Value)])
  else
    lblFilesize.Caption := 'Size: n/a';
end;

procedure TMainForm.txtHistoFileAcceptFileName(Sender: TObject;
  var Value: String);
var
  pic: TPicture;
  bmp: TBitmap;
begin
  FHistogram.StegHisto := nil;
  if Assigned(FStegHisto) then
    FStegHisto.Free;
  if FileExists(Value) then begin
    pic := TPicture.Create;
    Screen.Cursor := crHourGlass;
    try
      pic.LoadFromFile(Value);
      bmp := TBitmap.Create;
      try
        FStegHisto := TStegHisto.Create;
        bmp.Assign(pic.Graphic);
        FStegHisto.SetBitmap(bmp);
        FHistoGram.StegHisto := FStegHisto;
      finally
        bmp.Free;
      end;
    finally
      pic.Free;
      Screen.Cursor := crDefault;
    end;
  end;
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

procedure TMainForm.txtMessageToHideChange(Sender: TObject);
var
  s: string;
begin
  s := txtMessageToHide.Text;
  lblMsgsize.Caption := Format('Size: %d Byte', [Length(s) * SizeOf(Char)]);
end;

end.

