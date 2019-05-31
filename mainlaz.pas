unit MainLaz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCanvas, Forms, LMessages, Dialogs, Controls, SizeControl,
  StdCtrls, ExtCtrls, Spin, Buttons, Windows;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button10: TButton;
    Button9: TButton;
    Label1: TLabel;
    Shape1: TShape;
    SpinEdit1: TSpinEdit;
    VAR SizeCtrl: TSizeCtrl;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    procedure Button10Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var x: integer;
begin
  SizeCtrl := TSizeCtrl.Create(Form1);
  SizeCtrl.EditDisabled := True;
  SizeCtrl.SelectionKey := VK_CONTROL;
  SizeCtrl.BtnSize := 8;
  //..//SizeCtrl.ShowGrid:=True;
  SizeCtrl.GridSize := 8;
  {SizeCtrl.OnTargetChange := SizeCtrlTargetChange;
  SizeCtrl.OnDuringSizeMove := SizeCtrlDuring;
  SizeCtrl.OnEndSizeMove := SizeCtrlEnd;
  SizeCtrl.PopupMenu := sizePops;
  //to override behaviour of Pagecontrols so new pages can be selected ...
  SizeCtrl.OnMouseDown := SizeCtrlMouseDown;
  SizeCtrl.OnSetCursor := SizeCtrlSetCursor;
  SizeCtrl.OnKeyDown := SizeCtrlKeyDown;
  }
    for x := 0 to self.ControlCount-1 do
    sizectrl.RegisterControl(self.Controls[x]);
    SizeCtrl.ResizeFrameType:= TResizeFrameType.tszfButtons;
   // SizeCtrl.MovePanelCanvas.Brush.Style:= bsClear;
    SizeCtrl.Enabled := true;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  Shape1.Brush.Color := 536870912;
end;


end.

