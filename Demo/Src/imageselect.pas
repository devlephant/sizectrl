unit imageselect;

interface

uses
{$IFDEF FPC}
Classes, Graphics,
Controls, Forms, Dialogs, StdCtrls, Buttons,
ExtCtrls,
ExtDlgs, LCLTYPE
{$ELSE}
System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ExtDlgs, vcl.imaging.pngimage, vcl.imaging.jpeg, vcl.imaging.gifimg
{$ENDIF};

type
  TImSelect = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    BitBtn3: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn4: TBitBtn;
    opd1: TOpenPictureDialog;
    CheckBox1: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Execute(const Picture: TPicture): Boolean;
  end;

var
  ImSelect: TImSelect;

implementation
{$IFnDEF FPC}
        {$R *.dfm}
{$ELSE}
       {$R *.lfm}
{$ENDIF}

function TImSelect.Execute(const Picture: TPicture): Boolean;
begin
  Image1.Picture.Assign( Picture );
  Result := Self.ShowModal =  mrOk;
  if Result then
    Picture.Assign( Image1.Picture );
end;

procedure TImSelect.BitBtn1Click(Sender: TObject);
begin
    if opd1.Execute then
      Image1.Picture.LoadFromFile( opd1.FileName );
end;

procedure TImSelect.BitBtn2Click(Sender: TObject);
begin
  Image1.Picture.Graphic.Free;
end;

procedure TImSelect.CheckBox1Click(Sender: TObject);
begin
  Image1.Stretch := TCheckBox(Sender).Checked;
end;

end.
